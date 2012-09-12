module Volare.Handler.Workspace (
    getWorkspacesR,
    postWorkspacesR,
    getWorkspaceR
) where

import Control.Applicative ((<$>),
                            pure)
import Data.Monoid ((<>))
import Database.Persist (Entity(Entity),
                         insert,
                         selectList)
import Yesod.Core (defaultLayout)
import Yesod.Content (RepHtml)
import Yesod.Form (AForm,
                   Enctype,
                   FormResult(FormSuccess),
                   generateFormPost,
                   renderDivs,
                   runFormPost)
import Yesod.Handler (redirect)
import Yesod.Persist (get404,
                      runDB)
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


newWorkspaceAForm :: AForm Volare Volare NewWorkspace
newWorkspaceAForm = pure NewWorkspace


newWorkspaceForm :: Form NewWorkspace
newWorkspaceForm = renderDivs newWorkspaceAForm


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
    addStylesheet $ StaticR S.css_flight_css
    $(widgetFile "workspaces/show")
