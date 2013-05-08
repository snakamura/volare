module Volare.Handler.Workspace (
    getWorkspacesR,
    postWorkspacesR,
    getWorkspaceR,
    putWorkspaceR,
    deleteWorkspaceR,
    getWorkspaceFlightsR,
    postWorkspaceFlightsR,
    getWorkspaceCandidatesR
) where

import Control.Applicative ((<$>),
                            pure)
import Control.Arrow (first)
import Control.Monad (join)
import Control.Monad.Logger (MonadLogger)
import Control.Monad.Trans.Resource (MonadResource)
import Data.Aeson ((.=),
                   (.:))
import qualified Data.Aeson as JSON
import Data.List (minimumBy,
                  sortBy)
import qualified Data.Map as Map
import Data.Maybe (catMaybes)
import Data.Monoid ((<>),
                    mempty)
import Data.Ord (comparing)
import qualified Data.Text as T
import Data.Traversable (forM,
                         mapM)
import Database.Persist (Entity(Entity),
                         Key,
                         PersistMonadBackend,
                         PersistQuery,
                         SelectOpt(Asc),
                         (=.),
                         (==.),
                         delete,
                         deleteWhere,
                         insert,
                         insertUnique,
                         selectFirst,
                         selectList,
                         update)
import Database.Persist.Sql (SqlPersistT)
import Prelude hiding (mapM)
import Text.Blaze (Markup)
import Yesod.Core (HandlerSite,
                   WidgetT,
                   defaultLayout)
import Yesod.Core.Content (RepHtml)
import Yesod.Core.Handler (getRequest,
                           redirect,
                           reqToken)
import Yesod.Core.Json (parseJsonBody_)
import Yesod.Core.Widget (addScript,
                          addScriptRemote,
                          addStylesheet,
                          setTitle)
import Yesod.Form (Enctype,
                   FormResult(FormSuccess),
                   MForm,
                   generateFormPost,
                   renderDivs,
                   runFormPost)
import Yesod.Persist (get404,
                      runDB)

import qualified Volare.Config as Config
import Volare.Foundation
import Volare.Handler.Utils (addCommonLibraries)
import qualified Volare.Model as M
import Volare.Settings (widgetFile)
import qualified Volare.Static as S


data NewWorkspace = NewWorkspace


newWorkspaceForm :: Monad m =>
                    Markup ->
                    MForm m (FormResult NewWorkspace, WidgetT (HandlerSite m) IO ())
newWorkspaceForm = renderDivs $ pure NewWorkspace


getWorkspacesR :: Handler RepHtml
getWorkspacesR = do
    (workspaceWidget, enctype) <- generateFormPost $ newWorkspaceForm
    listWorkspaces workspaceWidget enctype


postWorkspacesR :: Handler RepHtml
postWorkspacesR = do
    ((result, workspaceWidget), enctype) <- runFormPost newWorkspaceForm
    case result of
      FormSuccess NewWorkspace -> do
          workspaceId <- runDB $ insert $ M.Workspace "Untitled"
          redirect $ WorkspaceR workspaceId
      _ -> listWorkspaces workspaceWidget enctype


listWorkspaces :: Widget ->
                  Enctype ->
                  Handler RepHtml
listWorkspaces workspaceWidget enctype = do
    workspaces <- runDB $ selectList [] []
    defaultLayout $ do
        setTitle "Workspaces - Volare"
        $(widgetFile "workspaces/index")


getWorkspaceR :: M.WorkspaceId ->
                 Handler RepHtml
getWorkspaceR workspaceId = do
    workspace <- runDB $ get404 workspaceId
    token <- reqToken <$> getRequest
    googleApiKey <- Config.googleApiKey <$> getConfig
    defaultLayout $ do
        setTitle "Workspace - Volare"
        addCommonLibraries
        addScriptRemote $ "//maps.googleapis.com/maps/api/js?key=" <> googleApiKey <> "&sensor=false"
        addScript $ StaticR S.js_common_js
        addScript $ StaticR S.js_volare_js
        addScript $ StaticR S.js_workspace_js
        addStylesheet $ StaticR S.css_common_css
        addStylesheet $ StaticR S.css_volare_css
        addStylesheet $ StaticR S.css_workspace_css
        $(widgetFile "workspaces/show")


data EditWorkspace = EditWorkspace T.Text

instance JSON.FromJSON EditWorkspace where
    parseJSON (JSON.Object o) = EditWorkspace <$> o .: "name"
    parseJSON _ = mempty


putWorkspaceR :: M.WorkspaceId ->
                 Handler JSON.Value
putWorkspaceR workspaceId = do
    EditWorkspace name <- parseJsonBody_
    workspace <- runDB $ do
        update workspaceId [M.WorkspaceName =. name]
        selectFirst [M.WorkspaceId ==. workspaceId] []
    return $ JSON.toJSON workspace


deleteWorkspaceR :: M.WorkspaceId ->
                    Handler JSON.Value
deleteWorkspaceR workspaceId = do
    runDB $ do
        delete workspaceId
        deleteWhere [M.WorkspaceFlightWorkspaceId ==. workspaceId]
    return $ JSON.toJSON ()


data WorkspaceFlight = WorkspaceFlight M.FlightId M.Flight T.Text

instance JSON.ToJSON WorkspaceFlight where
    toJSON (WorkspaceFlight id flight color) =
        JSON.object [
            "id" .= id,
            "name" .= M.flightName flight,
            "color" .= color
          ]


getWorkspaceFlightsR :: M.WorkspaceId ->
                        Handler JSON.Value
getWorkspaceFlightsR workspaceId = do
    flights <- runDB $ selectWorkspaceFlights workspaceId
    return $ JSON.toJSON flights


data NewWorkspaceFlight = NewWorkspaceFlight [M.FlightId]

instance JSON.FromJSON NewWorkspaceFlight where
    parseJSON (JSON.Object o) = NewWorkspaceFlight <$> o .: "flightIds"
    parseJSON _ = mempty


postWorkspaceFlightsR :: M.WorkspaceId ->
                         Handler JSON.Value
postWorkspaceFlightsR workspaceId = do
    NewWorkspaceFlight flightIds <- parseJsonBody_
    newWorkspaceFlights <- runDB $ forM flightIds $ \flightId -> do
        color <- nextColor workspaceId
        id <- insertUnique $ M.WorkspaceFlight workspaceId flightId color
        mapM selectWorkspaceFlight id
    return $ JSON.toJSON $ catMaybes newWorkspaceFlights


getWorkspaceCandidatesR :: M.WorkspaceId ->
                           Handler JSON.Value
getWorkspaceCandidatesR workspaceId = do
    flights <- runDB $ selectCandidateFlights workspaceId
    return $ JSON.toJSON flights


selectWorkspaceFlight :: (MonadResource m, MonadLogger m) =>
                         M.WorkspaceFlightId ->
                         SqlPersistT m (Maybe WorkspaceFlight)
selectWorkspaceFlight workspaceFlightId = do
    workspaceFlight <- selectFirst [M.WorkspaceFlightId ==. workspaceFlightId] []
    join <$> mapM selectWorkspaceFlight' workspaceFlight


selectWorkspaceFlights :: (MonadResource m, MonadLogger m) =>
                          M.WorkspaceId ->
                          SqlPersistT m [WorkspaceFlight]
selectWorkspaceFlights workspaceId = do
    workspaceFlights <- selectList [M.WorkspaceFlightWorkspaceId ==. workspaceId] []
    (sortBy (comparing name) . catMaybes) <$> mapM selectWorkspaceFlight' workspaceFlights
  where
    name (WorkspaceFlight _ flight _) = M.flightName flight


selectWorkspaceFlight' :: (MonadResource m, MonadLogger m) =>
                          Entity M.WorkspaceFlight ->
                          SqlPersistT m (Maybe WorkspaceFlight)
selectWorkspaceFlight' workspaceFlight = fmap makeWorkspaceFlight <$> getFlight workspaceFlight
  where
    getFlight (Entity _ (M.WorkspaceFlight _ flightId color)) = fmap (, color) <$> selectFirst [M.FlightId ==. flightId] []
    makeWorkspaceFlight (Entity id flight, color) = WorkspaceFlight id flight color


selectCandidateFlights :: PersistQuery m =>
                          Key (M.WorkspaceGeneric (PersistMonadBackend m)) ->
                          m [Entity (M.FlightGeneric (PersistMonadBackend m))]
selectCandidateFlights _ = selectList [] [Asc M.FlightName]


nextColor :: (MonadResource m, MonadLogger m) =>
             M.WorkspaceId ->
             SqlPersistT m T.Text
nextColor workspaceId = do
    let colors = [
            "red",
            "blue",
            "green",
            "yellow",
            "aqua",
            "fuchsia",
            "lime",
            "maroon",
            "navy",
            "olive",
            "purple",
            "silver",
            "teal"
          ]
    usedColors <- map color <$> selectWorkspaceFlights workspaceId
    let initialColorMap = Map.fromList $ zip colors $ zip (repeat (0 :: Int)) [0 :: Int ..]
        colorMap = foldr (Map.update (Just . first (+ 1))) initialColorMap usedColors
    return $ fst $ minimumBy (comparing snd) $ Map.assocs colorMap
  where
    color (WorkspaceFlight _ _ c) = c
