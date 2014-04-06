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

import Control.Applicative ((<$>),
                            (<*>),
                            pure)
import Data.Aeson ((.:),
                   (.:?))
import qualified Data.Aeson as JSON
import qualified Data.Aeson.Types as JSON
import qualified Data.HashMap.Strict as HashMap
import Data.Monoid ((<>),
                    mempty)
import qualified Data.Text as T
import Database.Persist (Entity,
                         entityKey,
                         entityVal)
import Text.Blaze.Html (Html,
                        toHtml)
import Yesod.Core (defaultLayout)
import Yesod.Core.Handler (provideRep,
                           selectRep)
import Yesod.Core.Json (requireJsonBody)
import Yesod.Core.Types (TypedContent)
import Yesod.Core.Widget (addScript,
                          addStylesheet,
                          setTitle)
import Yesod.Persist (runDB)

import qualified Volare.Domain as D
import Volare.Foundation
import Volare.Handler.Utils (addCommonLibraries,
                             addGoogleMapsApi,
                             maybeNotFound)
import qualified Volare.Model as M
import Volare.Settings (widgetFile)
import qualified Volare.Static as S
import qualified Volare.Widget as W


data NewWorkspace = NewWorkspace T.Text

instance JSON.FromJSON NewWorkspace where
    parseJSON (JSON.Object o) = NewWorkspace <$> o .: "name"
    parseJSON _ = mempty


getWorkspacesR :: Handler Html
getWorkspacesR = do
    workspaces <- runDB $ D.getWorkspaces
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
        workspaceId <- D.addWorkspace name
        D.getWorkspace workspaceId
    return $ JSON.toJSON workspace


getWorkspaceR :: M.WorkspaceId ->
                 Handler TypedContent
getWorkspaceR workspaceId = do
    maybeNotFound (runDB $ D.getWorkspace workspaceId) $ \workspaceEntity -> do
        let workspace = entityVal workspaceEntity
        selectRep $ do
            provideRep $ defaultLayout $ do
                setTitle $ toHtml $ M.workspaceName workspace <> " - Workspace - Volare"
                addCommonLibraries
                addGoogleMapsApi
                addScript $ StaticR S.js_common_js
                addScript $ StaticR S.js_volare_js
                addScript $ StaticR S.js_workspace_js
                addStylesheet $ StaticR S.css_common_css
                addStylesheet $ StaticR S.css_volare_css
                addStylesheet $ StaticR S.css_workspace_css
                $(widgetFile "workspaces/show")
            provideRep $ return $ JSON.toJSON workspaceEntity


data EditWorkspace = EditWorkspace (Maybe T.Text) (Maybe (Maybe M.RouteId))

instance JSON.FromJSON EditWorkspace where
    parseJSON (JSON.Object o) = EditWorkspace <$> o .:? "name"
                                              <*> o .:?? "routeId"
    parseJSON _ = mempty


(.:??) :: JSON.FromJSON a =>
          JSON.Object ->
          T.Text ->
          JSON.Parser (Maybe (Maybe a))
obj .:?? key = case HashMap.lookup key obj of
                 Nothing -> pure Nothing
                 Just v -> Just <$> JSON.parseJSON v


putWorkspaceR :: M.WorkspaceId ->
                 Handler JSON.Value
putWorkspaceR workspaceId = do
    EditWorkspace name routeId <- requireJsonBody
    workspace <- runDB $ do
        D.updateWorkspace workspaceId name routeId
        D.getWorkspace workspaceId
    return $ JSON.toJSON workspace


deleteWorkspaceR :: M.WorkspaceId ->
                    Handler JSON.Value
deleteWorkspaceR workspaceId = do
    runDB $ D.deleteWorkspace workspaceId
    return $ JSON.toJSON ()


getWorkspaceFlightsR :: M.WorkspaceId ->
                        Handler JSON.Value
getWorkspaceFlightsR workspaceId =
    runDB $ JSON.toJSON <$> D.getWorkspaceFlights workspaceId


data NewWorkspaceFlight = NewWorkspaceFlight [M.FlightId]

instance JSON.FromJSON NewWorkspaceFlight where
    parseJSON (JSON.Object o) = NewWorkspaceFlight <$> o .: "flightIds"
    parseJSON _ = mempty


postWorkspaceFlightsR :: M.WorkspaceId ->
                         Handler JSON.Value
postWorkspaceFlightsR workspaceId = do
    NewWorkspaceFlight flightIds <- requireJsonBody
    runDB $ JSON.toJSON <$> D.addWorkspaceFlight workspaceId flightIds


deleteWorkspaceFlightR :: M.WorkspaceId ->
                          M.FlightId ->
                          Handler JSON.Value
deleteWorkspaceFlightR workspaceId flightId = do
    runDB $ D.deleteWorkspaceFlight workspaceId flightId
    return $ JSON.toJSON ()


getWorkspaceCandidatesR :: M.WorkspaceId ->
                           Handler JSON.Value
getWorkspaceCandidatesR workspaceId =
    runDB $ JSON.toJSON <$> D.getWorkspaceCandidateFlights workspaceId
