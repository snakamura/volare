{-# LANGUAGE MultiParamTypeClasses,
             QuasiQuotes,
             TemplateHaskell,
             TypeFamilies #-}

import Yesod (RepHtml,
              Yesod,
              defaultLayout,
              mkYesod,
              parseRoutes,
              renderRoute,
              warpDebug,
              whamlet,
              yesodDispatch)

data Volare = Volare

mkYesod "Volare" [parseRoutes|
/ HomeR GET
|]

instance Yesod Volare


getHomeR :: Handler RepHtml
getHomeR = defaultLayout [whamlet|$newline never
Test|]


main :: IO ()
main = warpDebug 3000 Volare
