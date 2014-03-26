module Volare.Handler.Workspace (
    getWorkspacesR,
    postWorkspacesR,
    getWorkspaceR,
    putWorkspaceR,
    deleteWorkspaceR,
    getWorkspaceFlightsR,
    postWorkspaceFlightsR,
    deleteWorkspaceFlightR,
    getWorkspaceCandidatesR
) where

import Control.Applicative ((<$>))
import Control.Arrow (first)
import Control.Monad (join)
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
import Data.Time (addUTCTime)
import Data.Traversable (forM,
                         mapM)
import Database.Persist (Entity(Entity),
                         PersistEntityBackend,
                         PersistMonadBackend,
                         PersistQuery,
                         SelectOpt(Asc, Desc),
                         (=.),
                         (==.),
                         (<=.),
                         (>=.),
                         delete,
                         deleteWhere,
                         insert,
                         insertUnique,
                         selectFirst,
                         selectList,
                         update)
import Prelude hiding (mapM)
import Text.Blaze.Html (Html,
                        toHtml)
import Yesod.Core (defaultLayout)
import Yesod.Core.Handler (getRequest,
                           reqToken)
import Yesod.Core.Json (requireJsonBody)
import Yesod.Core.Widget (addScript,
                          addStylesheet,
                          setTitle)
import Yesod.Persist (get404,
                      runDB)

import Volare.Foundation
import Volare.Handler.Utils (addCommonLibraries,
                             addGoogleMapsApi)
import qualified Volare.Model as M
import Volare.Settings (widgetFile)
import qualified Volare.Static as S


data NewWorkspace = NewWorkspace T.Text

instance JSON.FromJSON NewWorkspace where
    parseJSON (JSON.Object o) = NewWorkspace <$> o .: "name"
    parseJSON _ = mempty


getWorkspacesR :: Handler Html
getWorkspacesR = do
    workspaces <- runDB $ selectList [] []
    defaultLayout $ do
        setTitle "Workspaces - Volare"
        addCommonLibraries
        addScript $ StaticR S.js_common_js
        addStylesheet $ StaticR S.css_common_css
        $(widgetFile "workspaces/index")


postWorkspacesR :: Handler JSON.Value
postWorkspacesR = do
  NewWorkspace name <- requireJsonBody
  workspace <- runDB $ do
      workspaceId <- insert $ M.Workspace name
      selectFirst [M.WorkspaceId ==. workspaceId] []
  return $ JSON.toJSON workspace


getWorkspaceR :: M.WorkspaceId ->
                 Handler Html
getWorkspaceR workspaceId = do
    workspace <- runDB $ get404 workspaceId
    token <- reqToken <$> getRequest
    defaultLayout $ do
        setTitle $ toHtml $ M.workspaceName workspace <> " - Workspace - Volare"
        addCommonLibraries
        addGoogleMapsApi
        addScript $ StaticR S.js_common_js
        addScript $ StaticR S.js_volare_js
        addScript $ StaticR S.js_workspace_js
        addStylesheet $ StaticR S.css_common_css
        addStylesheet $ StaticR S.css_volare_css
        addStylesheet $ StaticR S.css_workspace_css
        let options = $(widgetFile "elements/options")
            waypoint = $(widgetFile "elements/waypoint")
            weather = $(widgetFile "elements/weather")
        $(widgetFile "workspaces/show")


data EditWorkspace = EditWorkspace T.Text

instance JSON.FromJSON EditWorkspace where
    parseJSON (JSON.Object o) = EditWorkspace <$> o .: "name"
    parseJSON _ = mempty


putWorkspaceR :: M.WorkspaceId ->
                 Handler JSON.Value
putWorkspaceR workspaceId = do
    EditWorkspace name <- requireJsonBody
    workspace <- runDB $ do
        update workspaceId [M.WorkspaceName =. name]
        selectFirst [M.WorkspaceId ==. workspaceId] []
    return $ JSON.toJSON workspace


deleteWorkspaceR :: M.WorkspaceId ->
                    Handler JSON.Value
deleteWorkspaceR workspaceId = do
    runDB $ do
        deleteWhere [M.WorkspaceFlightWorkspaceId ==. workspaceId]
        delete workspaceId
    return $ JSON.toJSON ()


data WorkspaceFlight = WorkspaceFlight M.FlightId M.Flight T.Text

instance JSON.ToJSON WorkspaceFlight where
    toJSON (WorkspaceFlight workspaceId flight color) =
        JSON.object [
            "id" .= workspaceId,
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
    NewWorkspaceFlight flightIds <- requireJsonBody
    newWorkspaceFlights <- runDB $ forM flightIds $ \flightId -> do
        color <- nextColor workspaceId
        insertedFlightId <- insertUnique $ M.WorkspaceFlight workspaceId flightId color
        mapM selectWorkspaceFlight insertedFlightId
    return $ JSON.toJSON $ catMaybes newWorkspaceFlights


deleteWorkspaceFlightR :: M.WorkspaceId ->
                          M.FlightId ->
                          Handler JSON.Value
deleteWorkspaceFlightR workspaceId flightId = do
    runDB $ deleteWhere [M.WorkspaceFlightWorkspaceId ==. workspaceId,
                         M.WorkspaceFlightFlightId ==. flightId]
    return $ JSON.toJSON ()


getWorkspaceCandidatesR :: M.WorkspaceId ->
                           Handler JSON.Value
getWorkspaceCandidatesR workspaceId = do
    flights <- runDB $ selectCandidateFlights workspaceId
    return $ JSON.toJSON flights


selectWorkspaceFlight :: (Functor m, PersistQuery m, PersistMonadBackend m ~ PersistEntityBackend M.WorkspaceFlight) =>
                         M.WorkspaceFlightId ->
                         m (Maybe WorkspaceFlight)
selectWorkspaceFlight workspaceFlightId = do
    workspaceFlight <- selectFirst [M.WorkspaceFlightId ==. workspaceFlightId] []
    join <$> mapM selectWorkspaceFlight' workspaceFlight


selectWorkspaceFlights :: (Functor m, PersistQuery m, PersistMonadBackend m ~ PersistEntityBackend M.WorkspaceFlight) =>
                          M.WorkspaceId ->
                          m [WorkspaceFlight]
selectWorkspaceFlights workspaceId = do
    workspaceFlights <- selectList [M.WorkspaceFlightWorkspaceId ==. workspaceId] []
    (sortBy (comparing name) . catMaybes) <$> mapM selectWorkspaceFlight' workspaceFlights
  where
    name (WorkspaceFlight _ flight _) = M.flightName flight


selectWorkspaceFlight' :: (Functor m, PersistQuery m, PersistMonadBackend m ~ PersistEntityBackend M.WorkspaceFlight) =>
                          Entity M.WorkspaceFlight ->
                          m (Maybe WorkspaceFlight)
selectWorkspaceFlight' workspaceFlight = fmap makeWorkspaceFlight <$> getFlight workspaceFlight
  where
    getFlight (Entity _ (M.WorkspaceFlight _ flightId color)) = fmap (, color) <$> selectFirst [M.FlightId ==. flightId] []
    makeWorkspaceFlight (Entity flightId flight, color) = WorkspaceFlight flightId flight color


selectCandidateFlights :: (Functor m, PersistQuery m, PersistMonadBackend m ~ PersistEntityBackend M.Flight) =>
                          M.WorkspaceId ->
                          m [Entity M.Flight]
selectCandidateFlights workspaceId = do
    workspaceFlights <- selectWorkspaceFlights workspaceId
    case workspaceFlights of
      [] -> selectList [] [Desc M.FlightTime, Asc M.FlightName]
      _ -> let times = map (\(WorkspaceFlight _ flight _) -> M.flightTime flight) workspaceFlights
               start = addUTCTime (-6*60*60) $ minimum times
               end = addUTCTime (6*60*60) $ maximum times
               flightIds = map (\(WorkspaceFlight flightId _ _) -> flightId) workspaceFlights
               included (Entity flightId _) = flightId `elem` flightIds
           in filter (not . included) <$> selectList [M.FlightTime >=. start, M.FlightTime <=. end] [Asc M.FlightName]


nextColor :: (Functor m, PersistQuery m, PersistMonadBackend m ~ PersistEntityBackend M.Workspace) =>
             M.WorkspaceId ->
             m T.Text
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
