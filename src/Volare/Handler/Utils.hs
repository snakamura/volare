module Volare.Handler.Utils
    ( addJQuery
    , addJQueryUI
    , addUnderscore
    , addAngular
    , addAngularUIBootstrap
    , addGoogleMapsApi
    , addCommonLibraries
    , lookupIntegralGetParam
    , maybeNotFound
    ) where

import Data.Functor ((<$>))
import Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.Text.Read as T
import Yesod.Core
    ( MonadHandler
    , notFound
    )
import Yesod.Core.Handler (lookupGetParam)
import Yesod.Core.Widget
    ( addScript
    , addScriptRemote
    , addStylesheetRemote
    )

import qualified Volare.Config as Config
import Volare.Foundation
import qualified Volare.Static as S


addJQuery :: Widget
addJQuery = addScriptRemote "//code.jquery.com/jquery-2.1.1.min.js"


addJQueryUI :: Widget
addJQueryUI = do
    addScriptRemote "//ajax.googleapis.com/ajax/libs/jqueryui/1.11.2/jquery-ui.min.js"
    addStylesheetRemote "http://ajax.googleapis.com/ajax/libs/jqueryui/1.11.2/themes/ui-lightness/jquery-ui.css"


addBootstrap :: Widget
addBootstrap = do
    addScriptRemote "//netdna.bootstrapcdn.com/bootstrap/3.2.0/js/bootstrap.min.js"
    addStylesheetRemote "//netdna.bootstrapcdn.com/bootstrap/3.2.0/css/bootstrap.min.css"
    addStylesheetRemote "//netdna.bootstrapcdn.com/bootstrap/3.2.0/css/bootstrap-theme.min.css"


addUnderscore :: Widget
addUnderscore = do
    addScript $ StaticR S.js_lib_underscore_underscore_js
    addScript $ StaticR S.js_lib_underscore_string_underscore_string_js


addAngular :: Widget
addAngular = addScriptRemote "//ajax.googleapis.com/ajax/libs/angularjs/1.3.0/angular.min.js"


addAngularUIBootstrap :: Widget
addAngularUIBootstrap = addScript $ StaticR S.js_lib_angular_ui_bootstrap_bower_ui_bootstrap_tpls_js


addGoogleMapsApi :: Widget
addGoogleMapsApi = do
    googleApiKey <- Config.googleApiKey <$> getConfig
    addScriptRemote $ "//maps.googleapis.com/maps/api/js?key=" <> googleApiKey <> "&sensor=false"
    addScript $ StaticR S.js_lib_easy_markerwithlabel_markerwithlabel_js


addCommonLibraries :: Widget
addCommonLibraries = do
    addJQuery
    addJQueryUI
    addBootstrap
    addUnderscore
    addAngular


lookupIntegralGetParam :: (MonadHandler m, Integral a) =>
                          T.Text ->
                          m (Maybe a)
lookupIntegralGetParam name = (>>= readIntegral) <$> lookupGetParam name
  where
    readIntegral value = case T.decimal value of
                             Right (v, r) | T.null r -> Just v
                             _ -> Nothing


maybeNotFound :: MonadHandler m =>
                 m (Maybe a) ->
                 (a -> m b) ->
                 m b
maybeNotFound lookupAction execAction = lookupAction >>= maybe notFound execAction
