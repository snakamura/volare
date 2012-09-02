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
              whamletFile,
              yesodDispatch)

data Volare = Volare

mkYesod "Volare" [parseRoutes|
/ HomeR GET
/flights FlightsR GET
|]

instance Yesod Volare


getHomeR :: Handler RepHtml
getHomeR = defaultLayout $(whamletFile "templates/home.hamlet")


getFlightsR :: Handler RepHtml
getFlightsR = defaultLayout $(whamletFile "templates/flights/index.hamlet")


main :: IO ()
main = warpDebug 3000 Volare
