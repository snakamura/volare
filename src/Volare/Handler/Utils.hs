module Volare.Handler.Utils
    ( addJQueryUI
    , addBootstrap
    , addGoogleMapsApi
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


addJQueryUI :: Widget
addJQueryUI = addStylesheetRemote "//ajax.googleapis.com/ajax/libs/jqueryui/1.11.2/themes/ui-lightness/jquery-ui.css"


addBootstrap :: Widget
addBootstrap = do
    addStylesheetRemote "//netdna.bootstrapcdn.com/bootstrap/3.2.0/css/bootstrap.min.css"
    addStylesheetRemote "//netdna.bootstrapcdn.com/bootstrap/3.2.0/css/bootstrap-theme.min.css"


addGoogleMapsApi :: Widget
addGoogleMapsApi = do
    googleApiKey <- Config.googleApiKey <$> getConfig
    addScriptRemote $ "//maps.googleapis.com/maps/api/js?key=" <> googleApiKey <> "&sensor=false"
    addScript $ StaticR S.js_lib_easy_markerwithlabel_markerwithlabel_js


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
