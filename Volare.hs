{-# LANGUAGE FlexibleContexts,
             GADTs,
             MultiParamTypeClasses,
             OverloadedStrings,
             QuasiQuotes,
             TemplateHaskell,
             TypeFamilies #-}

import Control.Applicative ((<$>))
import qualified Data.Text as T
import Database.Persist (PersistEntity(..),
                         PersistEntityBackend,
                         PersistField(..))
import Database.Persist.TH (mkMigrate,
                            mkPersist,
                            persist,
                            share,
                            sqlSettings)
import Yesod (FormMessage,
              FormResult(FormSuccess),
              Html,
              MForm,
              RenderMessage,
              RepHtml,
              Yesod,
              areq,
              defaultFormMessage,
              defaultLayout,
              generateFormPost,
              mkYesod,
              parseRoutes,
              redirect,
              renderDivs,
              renderMessage,
              renderRoute,
              runFormPost,
              textField,
              warpDebug,
              whamletFile,
              yesodDispatch)


share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persist|
Flight
  name T.Text
  deriving Show
|]


data Volare = Volare

mkYesod "Volare" [parseRoutes|
/ HomeR GET
/flights FlightsR GET POST
|]

instance Yesod Volare

instance RenderMessage Volare FormMessage where
    renderMessage _ _ = defaultFormMessage


getHomeR :: Handler RepHtml
getHomeR = defaultLayout $(whamletFile "templates/home.hamlet")


flightForm :: Html ->
              MForm Volare Volare (FormResult Flight, Widget)
flightForm = renderDivs $ Flight <$> areq textField "Name" Nothing


getFlightsR :: Handler RepHtml
getFlightsR = do
  let flights = [Flight "<Test Flight>"]
  (newFlightWidget, enctype) <- generateFormPost flightForm
  defaultLayout $(whamletFile "templates/flights/index.hamlet")


postFlightsR :: Handler RepHtml
postFlightsR = do
  ((result, widget), enctype) <- runFormPost flightForm
  case result of
    FormSuccess flight -> redirect FlightsR
    _ -> redirect FlightsR


main :: IO ()
main = warpDebug 3000 Volare
