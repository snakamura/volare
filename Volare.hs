{-# LANGUAGE MultiParamTypeClasses,
             OverloadedStrings,
             QuasiQuotes,
             TemplateHaskell,
             TypeFamilies #-}

import qualified Data.Text as T
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


data Flight = Flight {
  name :: T.Text
} deriving Show


getFlightsR :: Handler RepHtml
getFlightsR = do
  let flights = [Flight "<Test Flight>"]
  defaultLayout $(whamletFile "templates/flights/index.hamlet")


main :: IO ()
main = warpDebug 3000 Volare
