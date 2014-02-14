module Volare.Handler.Utils (
    addJQuery,
    addJQueryUI,
    addUnderscore,
    addCommonLibraries
) where

import Yesod.Core.Widget (addScript,
                          addScriptRemote,
                          addStylesheetRemote)

import Volare.Foundation
import qualified Volare.Static as S


addJQuery :: Widget
addJQuery = addScriptRemote "//code.jquery.com/jquery-2.1.0.min.js"


addJQueryUI :: Widget
addJQueryUI = do
  addScriptRemote "//ajax.googleapis.com/ajax/libs/jqueryui/1.10.4/jquery-ui.min.js"
  addStylesheetRemote $ "http://ajax.googleapis.com/ajax/libs/jqueryui/1.10.4/themes/ui-lightness/jquery-ui.css"


addBootstrap :: Widget
addBootstrap = do
  addScriptRemote "//netdna.bootstrapcdn.com/bootstrap/3.1.1/js/bootstrap.min.js"
  addStylesheetRemote "//netdna.bootstrapcdn.com/bootstrap/3.1.1/css/bootstrap.min.css"
  addStylesheetRemote "//netdna.bootstrapcdn.com/bootstrap/3.1.1/css/bootstrap-theme.min.css"


addUnderscore :: Widget
addUnderscore = do
    addScript $ StaticR S.js_underscore_min_js
    addScript $ StaticR S.js_underscore_string_min_js


addCommonLibraries :: Widget
addCommonLibraries = do
    addJQuery
    addJQueryUI
    addBootstrap
    addUnderscore
