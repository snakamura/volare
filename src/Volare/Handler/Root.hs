module Volare.Handler.Root
    ( getRootR
    ) where

import Network.HTTP.Types (temporaryRedirect307)
import Text.Blaze.Html (Html)
import Yesod.Core.Handler (redirectWith)

import Volare.Foundation


getRootR :: Handler Html
getRootR = redirectWith temporaryRedirect307 FlightsR
