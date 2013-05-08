module Volare.Handler.Root (
    getRootR
) where

import Yesod.Core (defaultLayout)
import Yesod.Core.Content (RepHtml)
import Yesod.Core.Widget (setTitle)

import Volare.Foundation
import Volare.Settings (widgetFile)


getRootR :: Handler RepHtml
getRootR =
    defaultLayout $ do
        setTitle "Volare"
        $(widgetFile "root")
