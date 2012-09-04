{-# LANGUAGE FlexibleContexts,
             GADTs,
             MultiParamTypeClasses,
             OverloadedStrings,
             QuasiQuotes,
             TemplateHaskell,
             TypeFamilies #-}

module Main (main) where

import qualified Codec.IGC as IGC
import Control.Applicative ((<$>),
                            (<*>))
import Control.Exception.Lifted (handle)
import Control.Monad.Logger (LogLevel(LevelDebug))
import Control.Monad.IO.Class (liftIO)
import Data.Conduit (($$),
                     runResourceT)
import Data.Conduit.Attoparsec (ParseError,
                                sinkParser)
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
                   logDebug,
                   renderRoute,
                   yesodDispatch)
import Yesod.Content (RepHtml)
import Yesod.Dispatch (mkYesod,
                       parseRoutes)
import Yesod.Form (AForm,
                   Enctype,
                   FormMessage,
                   FormResult(FormSuccess),
                   MForm,
                   areq,
                   defaultFormMessage,
                   fileAFormReq,
                   generateFormPost,
                   renderDivs,
                   runFormPost,
                   textField)
import Yesod.Handler (getYesod,
                      redirect)
import Yesod.Persist (YesodPersist(..),
                      get404)
import Yesod.Request (FileInfo,
                      fileName,
                      fileSource)
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
    logLevel _ = LevelDebug

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


data NewFlight = NewFlight Flight FileInfo


newFlightAForm :: AForm Volare Volare NewFlight
newFlightAForm = NewFlight <$> (Flight <$> areq textField "Name" Nothing)
                           <*> fileAFormReq "File"


newFlightForm :: Html ->
                 MForm Volare Volare (FormResult NewFlight, Widget)
newFlightForm = renderDivs newFlightAForm


getFlightsR :: Handler RepHtml
getFlightsR = do
  (flightWidget, enctype) <- generateFormPost $ newFlightForm
  listFlights flightWidget enctype


postFlightsR :: Handler RepHtml
postFlightsR = do
  ((result, flightWidget), enctype) <- runFormPost newFlightForm
  case result of
    FormSuccess (NewFlight flight file) ->
        let handler :: ParseError ->
                       Handler RepHtml
            handler e = do
                 $(logDebug) $ T.pack $ show e
                 listFlights flightWidget enctype
        in handle handler $ do
                 igc <- liftIO $ runResourceT $ fileSource file $$ sinkParser (IGC.igc $ fileName file)
                 $(logDebug) $ T.pack $ show igc
                 flightId <- runDB $ insert flight
                 redirect $ FlightR flightId
    _ -> listFlights flightWidget enctype


listFlights :: Widget ->
               Enctype ->
               Handler RepHtml
listFlights flightWidget enctype = do
  flights <- runDB $ selectList [] []
  defaultLayout $(whamletFile "templates/flights/index.hamlet")


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
  editFlight flightId flightWidget enctype


postFlightEditR :: FlightId ->
                   Handler RepHtml
postFlightEditR flightId = do
  ((result, flightWidget), enctype) <- runFormPost $ flightForm Nothing
  case result of
    FormSuccess flight -> do
                runDB $ replace flightId flight
                redirect $ FlightR flightId
    _ -> editFlight flightId flightWidget enctype


editFlight :: FlightId ->
              Widget ->
              Enctype ->
              Handler RepHtml
editFlight flightId flightWidget enctype = defaultLayout $(whamletFile "templates/flights/edit.hamlet")


main :: IO ()
main = do
  config <- loadConfig "config/config.yml"
  withSqlitePool (sqlitePath config) (sqliteConnectionPoolCount config) $ \pool -> do
         runSqlPool (runMigration migrateAll) pool
         warpDebug 3000 $ Volare pool
