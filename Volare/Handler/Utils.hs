module Volare.Handler.Utils (
    addJQuery,
    addJQueryUI,
    addUnderscore,
    addCommonLibraries
) where

import Yesod.Widget (addScript,
                     addScriptRemote,
                     addStylesheetRemote)

import Volare.Foundation
import qualified Volare.Static as S


addJQuery :: Widget
addJQuery = addScriptRemote "//ajax.googleapis.com/ajax/libs/jquery/1.8.1/jquery.min.js"


addJQueryUI :: Widget
addJQueryUI = do
  addScriptRemote "//ajax.googleapis.com/ajax/libs/jqueryui/1.8.23/jquery-ui.min.js"
  addStylesheetRemote $ "http://ajax.googleapis.com/ajax/libs/jqueryui/1.8.23/themes/ui-lightness/jquery-ui.css"


addUnderscore :: Widget
addUnderscore = do
    addScript $ StaticR S.js_underscore_min_js
    addScript $ StaticR S.js_underscore_string_min_js


addCommonLibraries :: Widget
addCommonLibraries = do
    addJQuery
    addJQueryUI
    addUnderscore
