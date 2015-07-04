module Volare.Handler.Forcast
    ( getForcastR
    ) where

import Text.Blaze.Html (Html)
import Yesod.Core (defaultLayout)
import Yesod.Core.Widget
    ( addStylesheet
    , setTitle
    )

import Volare.Foundation
import Volare.Handler.Utils
    ( addCommonLibraries
    , addGoogleApiKey
    , addJQueryUI
    , addRequireJS
    )
import Volare.Settings (widgetFile)
import qualified Volare.Static as S
import qualified Volare.Widget as W


getForcastR :: Handler Html
getForcastR = defaultLayout $ do
    setTitle "Forcast - Volare"
    addCommonLibraries
    addGoogleApiKey
    addJQueryUI
    addRequireJS "forcast/index"
    addStylesheet $ StaticR S.css_common_css
    $(widgetFile "forcast/index")
