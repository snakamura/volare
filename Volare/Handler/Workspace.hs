module Volare.Handler.Workspace (
    getWorkspacesR,
    postWorkspacesR,
    getWorkspaceR,
    getWorkspaceFlightsR,
    postWorkspaceFlightsR
) where

import Control.Applicative ((<$>),
                            pure)
import qualified Data.Aeson as JSON
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Foldable (forM_)
import Data.Maybe (catMaybes)
import Data.Monoid ((<>))
import qualified Data.Text.Encoding as T
import Data.Traversable (mapM)
import Database.Persist (Entity(Entity),
                         Key,
                         PersistQuery,
                         (==.),
                         insert,
                         insertUnique,
                         selectFirst,
                         selectList)
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


data WorkspaceFlight = WorkspaceFlight [M.FlightId] deriving Show


workspaceFlightForm :: M.WorkspaceId ->
                       Form WorkspaceFlight
workspaceFlightForm _ =
    let options = runDB $ (mkOptionList . map option) <$> selectList [] []
        option (Entity id flight) = Option (M.flightName flight) id (T.decodeUtf8 $ B.concat $ BL.toChunks $ JSON.encode id)
    in renderDivs $ WorkspaceFlight <$> areq (multiSelectField options) ("Flights" { fsName = Just "flights" }) Nothing


getWorkspaceFlightsR :: M.WorkspaceId ->
                        Handler RepJson
getWorkspaceFlightsR workspaceId = do
  flights <- runDB $ flightsInWorkspace workspaceId
  jsonToRepJson flights


postWorkspaceFlightsR :: M.WorkspaceId ->
                         Handler RepJson
postWorkspaceFlightsR workspaceId = do
  ((result, workspaceFlightWidget), enctype) <- runFormPost $ workspaceFlightForm workspaceId
  case result of
    FormSuccess (WorkspaceFlight flightIds) ->
        do runDB $ forM_ flightIds $ \flightId ->
               insertUnique $ M.WorkspaceFlight workspaceId flightId
           jsonToRepJson flightIds
    _ -> invalidArgs ["flights"]


flightsInWorkspace :: PersistQuery backend m =>
                      Key backend (M.WorkspaceGeneric backend) ->
                      backend m [Entity (M.FlightGeneric backend)]
flightsInWorkspace workspaceId = do
  workspaceFlights <- selectList [M.WorkspaceFlightWorkspaceId ==. workspaceId] []
  catMaybes <$> mapM getFlight workspaceFlights
    where
      getFlight (Entity _ (M.WorkspaceFlight _ flightId)) = selectFirst [M.FlightId ==. flightId] []
