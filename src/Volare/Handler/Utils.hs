module Volare.Handler.Utils
    ( addRequireJS
    , addNormalize
    , addJQueryUI
    , addBootstrap
    , addGoogleApiKey
    , addCommonLibraries
    , lookupIntegralGetParam
    , maybeNotFound
    ) where

import qualified Data.Text as T
import qualified Data.Text.Read as T
import Text.Julius (julius)
import Yesod.Core
    ( MonadHandler
    , notFound
    )
import Yesod.Core.Handler (lookupGetParam)
import Yesod.Core.Widget
    ( addScriptRemoteAttrs
    , addStylesheetRemote
    , toWidget
    )

import Volare.Foundation
import qualified Volare.Settings as Settings


addRequireJS :: T.Text ->
                Widget
addRequireJS name = addScriptRemoteAttrs "//cdnjs.cloudflare.com/ajax/libs/require.js/2.1.15/require.min.js" [("data-main", "/static/js/" <> name)]


addNormalize :: Widget
addNormalize = addStylesheetRemote "//cdnjs.cloudflare.com/ajax/libs/normalize/3.0.1/normalize.min.css"


addJQueryUI :: Widget
addJQueryUI = addStylesheetRemote "//ajax.googleapis.com/ajax/libs/jqueryui/1.11.2/themes/ui-lightness/jquery-ui.css"


addBootstrap :: Widget
addBootstrap = do
    addStylesheetRemote "//netdna.bootstrapcdn.com/bootstrap/3.2.0/css/bootstrap.min.css"
    addStylesheetRemote "//netdna.bootstrapcdn.com/bootstrap/3.2.0/css/bootstrap-theme.min.css"


addGoogleApiKey :: Widget
addGoogleApiKey = do
    googleApiKey <- Settings.googleApiKey <$> getSettings
    toWidget [julius|var googleApiKey = #{googleApiKey};|]


addCommonLibraries :: Widget
addCommonLibraries = do
    addNormalize
    addBootstrap


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
