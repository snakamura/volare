module Volare.Handler.Workspace
    ( getWorkspacesR
    , postWorkspacesR
    , getWorkspaceR
    , putWorkspaceR
    , deleteWorkspaceR
    , getWorkspaceFlightsR
    , postWorkspaceFlightsR
    , deleteWorkspaceFlightR
    , getWorkspaceCandidatesR
    ) where

import Data.Aeson
    ( (.:)
    , (.:?)
    )
import qualified Data.Aeson as JSON
import qualified Data.Text as T
import Database.Persist (entityVal)
import Text.Blaze.Html (toHtml)
import Yesod.Core (defaultLayout)
import Yesod.Core.Handler
    ( provideRep
    , selectRep
    )
import Yesod.Core.Json (requireCheckJsonBody)
import Yesod.Core.Types (TypedContent)
import Yesod.Core.Widget
    ( addStylesheet
    , setTitle
    )
import Yesod.Persist (runDB)

import qualified Volare.Domain as D
import Volare.Foundation
import Volare.Handler.Utils
    ( addCommonLibraries
    , addGoogleApiKey
    , addJQueryUI
    , addRequireJS
    , maybeNotFound
    )
import qualified Volare.Model as M
import Volare.Settings (widgetFile)
import qualified Volare.Static as S
import qualified Volare.Widget as W


data NewWorkspace = NewWorkspace T.Text

instance JSON.FromJSON NewWorkspace where
    parseJSON (JSON.Object o) = NewWorkspace <$> o .: "name"
    parseJSON _ = mempty


getWorkspacesR :: Handler TypedContent
getWorkspacesR =
    selectRep $ do
        provideRep $ defaultLayout $ do
            setTitle "Workspaces - Volare"
            addCommonLibraries
            addRequireJS "workspaces/index"
            addStylesheet $ StaticR S.css_common_css
            $(widgetFile "workspaces/index")
        provideRep $ runDB $ JSON.toJSON <$> D.getWorkspaces


postWorkspacesR :: Handler JSON.Value
postWorkspacesR = do
    NewWorkspace name <- requireCheckJsonBody
    workspace <- runDB $ do
        workspaceId <- D.addWorkspace name
        D.getWorkspace workspaceId
    return $ JSON.toJSON workspace


getWorkspaceR :: M.WorkspaceId ->
                 Handler TypedContent
getWorkspaceR workspaceId =
    maybeNotFound (runDB $ D.getWorkspace workspaceId) $ \workspaceEntity -> do
        let workspace = entityVal workspaceEntity
        selectRep $ do
            provideRep $ defaultLayout $ do
                setTitle $ toHtml $ M.workspaceName workspace <> " - Workspace - Volare"
                addCommonLibraries
                addGoogleApiKey
                addJQueryUI
                addRequireJS "workspaces/show"
                addStylesheet $ StaticR S.css_common_css
                $(widgetFile "workspaces/show")
            provideRep $ return $ JSON.toJSON workspaceEntity


data EditWorkspace = EditWorkspace (Maybe T.Text) (Maybe (Maybe M.RouteId))

instance JSON.FromJSON EditWorkspace where
    parseJSON (JSON.Object o) = EditWorkspace <$> o .:? "name"
                                              <*> o .:? "routeId"
    parseJSON _ = mempty


putWorkspaceR :: M.WorkspaceId ->
                 Handler JSON.Value
putWorkspaceR workspaceId = do
    EditWorkspace name routeId <- requireCheckJsonBody
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
    NewWorkspaceFlight flightIds <- requireCheckJsonBody
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
