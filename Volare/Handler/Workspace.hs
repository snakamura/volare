module Volare.Handler.Workspace (
    getWorkspacesR,
    postWorkspacesR,
    getWorkspaceR,
    getWorkspaceFlightsR,
    postWorkspaceFlightsR,
    getWorkspaceCandidatesR
) where

import Control.Applicative ((<$>),
                            pure)
import Control.Monad (join)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Logger (MonadLogger)
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Trans.Resource (MonadThrow,
                                     MonadUnsafeIO)
import Data.Aeson ((.=))
import qualified Data.Aeson as JSON
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.List (sortBy)
import Data.Maybe (catMaybes)
import Data.Monoid ((<>))
import Data.Ord (comparing)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Traversable (forM,
                         mapM)
import Database.Persist (Entity(Entity),
                         Key,
                         PersistQuery,
                         SelectOpt(Asc),
                         (==.),
                         insert,
                         insertUnique,
                         selectFirst,
                         selectList)
import Database.Persist.GenericSql (SqlPersist)
import Prelude hiding (mapM)
import Yesod.Core (defaultLayout)
import Yesod.Content (RepHtml,
                      RepJson)
import Yesod.Form (Enctype,
                   FormResult(FormSuccess),
                   Option(Option),
                   areq,
                   fsName,
                   generateFormPost,
                   mkOptionList,
                   multiSelectField,
                   renderDivs,
                   runFormPost)
import Yesod.Handler (getRequest,
                      invalidArgs,
                      redirect)
import Yesod.Json (jsonToRepJson)
import Yesod.Persist (get404,
                      runDB)
import Yesod.Request (reqToken)
import Yesod.Widget (addScript,
                     addScriptRemote,
                     addStylesheet,
                     addStylesheetRemote,
                     setTitle)

import qualified Volare.Config as Config
import Volare.Foundation
import qualified Volare.Model as M
import Volare.Settings (widgetFile)
import qualified Volare.Static as S


data NewWorkspace = NewWorkspace


newWorkspaceForm :: Form NewWorkspace
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
    addScriptRemote "//ajax.googleapis.com/ajax/libs/jquery/1.8.1/jquery.min.js"
    addScriptRemote "//ajax.googleapis.com/ajax/libs/jqueryui/1.8.23/jquery-ui.min.js"
    addScriptRemote $ "//maps.googleapis.com/maps/api/js?key=" <> googleApiKey <> "&sensor=false"
    addStylesheetRemote $ "http://ajax.googleapis.com/ajax/libs/jqueryui/1.8.23/themes/ui-lightness/jquery-ui.css"
    addScript $ StaticR S.js_underscore_min_js
    addScript $ StaticR S.js_underscore_string_min_js
    addScript $ StaticR S.js_volare_js
    addScript $ StaticR S.js_workspace_js
    addStylesheet $ StaticR S.css_common_css
    addStylesheet $ StaticR S.css_volare_css
    addStylesheet $ StaticR S.css_workspace_css
    $(widgetFile "workspaces/show")


data NewWorkspaceFlight = NewWorkspaceFlight [M.FlightId]


newWorkspaceFlightForm :: M.WorkspaceId ->
                          Form NewWorkspaceFlight
newWorkspaceFlightForm workspaceId =
    let options = runDB $ (mkOptionList . map option) <$> selectCandidateFlights workspaceId
        option (Entity id flight) = Option (M.flightName flight) id (T.decodeUtf8 $ B.concat $ BL.toChunks $ JSON.encode id)
    in renderDivs $ NewWorkspaceFlight <$> areq (multiSelectField options) ("Flights" { fsName = Just "flights" }) Nothing


data WorkspaceFlight = WorkspaceFlight M.FlightId M.Flight T.Text

instance JSON.ToJSON WorkspaceFlight where
    toJSON (WorkspaceFlight id flight color) =
        JSON.object [
                 "id" .= id,
                 "name" .= M.flightName flight,
                 "color" .= color
                ]


getWorkspaceFlightsR :: M.WorkspaceId ->
                        Handler RepJson
getWorkspaceFlightsR workspaceId = do
  flights <- runDB $ selectWorkspaceFlights workspaceId
  jsonToRepJson flights


postWorkspaceFlightsR :: M.WorkspaceId ->
                         Handler RepJson
postWorkspaceFlightsR workspaceId = do
  ((result, workspaceFlightWidget), enctype) <- runFormPost $ newWorkspaceFlightForm workspaceId
  case result of
    FormSuccess (NewWorkspaceFlight flightIds) ->
        do workspaceFlights <- runDB $ forM flightIds $ \flightId ->
               do let color = "red" :: T.Text
                  id <- insertUnique $ M.WorkspaceFlight workspaceId flightId color
                  mapM selectWorkspaceFlight id
           jsonToRepJson $ catMaybes workspaceFlights
    _ -> invalidArgs ["flights"]


getWorkspaceCandidatesR :: M.WorkspaceId ->
                           Handler RepJson
getWorkspaceCandidatesR workspaceId = do
  flights <- runDB $ selectCandidateFlights workspaceId
  jsonToRepJson flights


selectWorkspaceFlight :: (MonadIO m, MonadBaseControl IO m, MonadLogger m, MonadUnsafeIO m, MonadThrow m) =>
                         M.WorkspaceFlightId ->
                         SqlPersist m (Maybe WorkspaceFlight)
selectWorkspaceFlight workspaceFlightId = do
    workspaceFlight <- selectFirst [M.WorkspaceFlightId ==. workspaceFlightId] []
    join <$> mapM selectWorkspaceFlight' workspaceFlight


selectWorkspaceFlights :: (MonadIO m, MonadBaseControl IO m, MonadLogger m, MonadUnsafeIO m, MonadThrow m) =>
                          M.WorkspaceId ->
                          SqlPersist m [WorkspaceFlight]
selectWorkspaceFlights workspaceId = do
  workspaceFlights <- selectList [M.WorkspaceFlightWorkspaceId ==. workspaceId] []
  (sortBy (comparing name) . catMaybes) <$> mapM selectWorkspaceFlight' workspaceFlights
    where
      name (WorkspaceFlight _ flight _) = M.flightName flight


selectWorkspaceFlight' :: (MonadIO m, MonadBaseControl IO m, MonadLogger m, MonadUnsafeIO m, MonadThrow m) =>
                          Entity M.WorkspaceFlight ->
                          SqlPersist m (Maybe WorkspaceFlight)
selectWorkspaceFlight' workspaceFlight = fmap makeWorkspaceFlight <$> getFlight workspaceFlight
    where
      getFlight (Entity _ (M.WorkspaceFlight _ flightId color)) = fmap (, color) <$> selectFirst [M.FlightId ==. flightId] []
      makeWorkspaceFlight (Entity id flight, color) = WorkspaceFlight id flight color


selectCandidateFlights :: PersistQuery backend m =>
                          Key backend (M.WorkspaceGeneric backend) ->
                          backend m [Entity (M.FlightGeneric backend)]
selectCandidateFlights _ = selectList [] [Asc M.FlightName]
