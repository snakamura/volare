{-# LANGUAGE FlexibleContexts,
             GADTs,
             MultiParamTypeClasses,
             OverloadedStrings,
             QuasiQuotes,
             TemplateHaskell,
             TypeFamilies #-}

module Main (main) where

import Control.Applicative ((<$>))
import qualified Data.Text as T
import Database.Persist (PersistEntity(..),
                         PersistEntityBackend,
                         PersistField(..))
import Database.Persist (Entity(Entity),
                         insert,
                         replace,
                         selectList)
import Database.Persist.GenericSql (ConnectionPool,
                                    SqlPersist,
                                    runMigration,
                                    runSqlPool)
import Database.Persist.Sqlite (withSqlitePool)
import Database.Persist.TH (mkMigrate,
                            mkPersist,
                            persist,
                            share,
                            sqlSettings)
import Text.Blaze.Html (Html)
import Text.Shakespeare.I18N (RenderMessage,
                              renderMessage)
import Web.ClientSession (getKey)
import Yesod (warpDebug)
import Yesod.Core (Yesod(..),
                   clientSessionBackend,
                   defaultLayout,
                   renderRoute,
                   yesodDispatch)
import Yesod.Content (RepHtml)
import Yesod.Dispatch (mkYesod,
                       parseRoutes)
import Yesod.Form (AForm,
                   FormMessage,
                   FormResult(FormSuccess),
                   MForm,
                   areq,
                   defaultFormMessage,
                   generateFormPost,
                   renderDivs,
                   runFormPost,
                   textField)
import Yesod.Handler (getYesod,
                      redirect)
import Yesod.Persist (YesodPersist(..),
                      get404)
import Yesod.Widget (whamletFile)

import Volare.Config (Config,
                      loadConfig,
                      sqlitePath,
                      sqliteConnectionPoolCount)


share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persist|
Flight
  name T.Text
  deriving Show
|]


data Volare = Volare ConnectionPool


mkYesod "Volare" [parseRoutes|
/ RootR GET
/flights FlightsR GET POST
/flights/#FlightId FlightR GET
/flights/#FlightId/edit FlightEditR GET POST
|]


instance Yesod Volare where
    makeSessionBackend _ = do
      key <- getKey "config/client_session_key.aes"
      return $ Just $ clientSessionBackend key 120


instance YesodPersist Volare where
    type YesodPersistBackend Volare = SqlPersist

    runDB action = do
      Volare pool <- getYesod
      runSqlPool action pool


instance RenderMessage Volare FormMessage where
    renderMessage _ _ = defaultFormMessage


getRootR :: Handler RepHtml
getRootR = defaultLayout $(whamletFile "templates/root.hamlet")


flightAForm :: Maybe Flight ->
               AForm Volare Volare Flight
flightAForm flight = Flight <$> areq textField "Name" (flightName <$> flight)


flightForm :: Maybe Flight ->
              Html ->
              MForm Volare Volare (FormResult Flight, Widget)
flightForm = renderDivs . flightAForm


getFlightsR :: Handler RepHtml
getFlightsR = do
  flights <- runDB $ selectList [] []
  (flightWidget, enctype) <- generateFormPost $ flightForm Nothing
  defaultLayout $(whamletFile "templates/flights/index.hamlet")


postFlightsR :: Handler RepHtml
postFlightsR = do
  ((result, widget), enctype) <- runFormPost $ flightForm Nothing
  case result of
    FormSuccess flight -> do
                 runDB $ insert flight
                 redirect FlightsR
    _ -> redirect FlightsR


getFlightR :: FlightId ->
              Handler RepHtml
getFlightR flightId = do
  flight <- runDB $ get404 flightId
  defaultLayout $(whamletFile "templates/flights/show.hamlet")


getFlightEditR :: FlightId ->
                  Handler RepHtml
getFlightEditR flightId = do
  flight <- runDB $ get404 flightId
  (flightWidget, enctype) <- generateFormPost $ flightForm $ Just flight
  defaultLayout $(whamletFile "templates/flights/edit.hamlet")


postFlightEditR :: FlightId ->
                Handler RepHtml
postFlightEditR flightId = do
  ((result, flightWidget), enctype) <- runFormPost $ flightForm Nothing
  case result of
    FormSuccess flight -> do
                runDB $ replace flightId flight
                redirect $ FlightR flightId
    _ -> defaultLayout $(whamletFile "templates/flights/edit.hamlet")


main :: IO ()
main = do
  config <- loadConfig "config/config.yml"
  withSqlitePool (sqlitePath config) (sqliteConnectionPoolCount config) $ \pool -> do
         runSqlPool (runMigration migrateAll) pool
         warpDebug 3000 $ Volare pool
