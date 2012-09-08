{-# LANGUAGE FlexibleContexts,
             FlexibleInstances,
             GADTs,
             MultiParamTypeClasses,
             OverloadedStrings,
             QuasiQuotes,
             TemplateHaskell,
             TypeFamilies,
             TypeSynonymInstances #-}

module Main (main) where

import qualified Codec.IGC as IGC
import Control.Applicative ((<$>))
import Control.Exception.Lifted (handle)
import Control.Monad.Logger (LogLevel(LevelDebug))
import Control.Monad.IO.Class (liftIO)
import Data.Aeson ((.=))
import qualified Data.Aeson as JSON
import Data.Conduit (($$),
                     runResourceT)
import Data.Conduit.Attoparsec (ParseError,
                                sinkParser)
import Data.Foldable (forM_)
import Data.Monoid ((<>))
import qualified Data.Text as T
import Data.Time (Day,
                  UTCTime(UTCTime),
                  formatTime)
import Database.Persist (Entity(Entity),
                         PersistEntity(..),
                         PersistEntityBackend,
                         PersistField(..),
                         SelectOpt(Asc),
                         (=.),
                         (==.),
                         insert,
                         update,
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
import System.Locale (defaultTimeLocale)
import Text.Blaze.Html (Html)
import Text.Printf (printf)
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
import Yesod.Content (RepHtml,
                      RepHtmlJson)
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
import Yesod.Json (defaultLayoutJson)
import Yesod.Persist (YesodPersist(..),
                      get404)
import Yesod.Request (FileInfo,
                      fileName,
                      fileSource)
import Yesod.Static (Static)
import Yesod.Widget (addScript,
                     addScriptRemote,
                     addStylesheet,
                     setTitle,
                     whamletFile)

import Volare.Config (Config,
                      loadConfig)
import qualified Volare.Config as Config
import Volare.Static (staticSite)
import qualified Volare.Static as S


share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persist|
Flight
  name T.Text
  date Day
  deriving Show
Record
  flightId FlightId
  index Int
  time UTCTime
  latitude Double
  longitude Double
  altitude Double
  deriving Show
|]

instance JSON.ToJSON Record where
    toJSON record = JSON.object [
                             "time" .= recordTime record,
                             "latitude" .= recordLatitude record,
                             "longitude" .= recordLongitude record,
                             "altitude" .= recordAltitude record
                            ]


data Volare = Volare {
    volareConfig         :: Config,
    volareConnectionPool :: ConnectionPool,
    volareStatic         :: Static
}


mkYesod "Volare" [parseRoutes|
/ RootR GET
/flights FlightsR GET POST
/flights/#FlightId FlightR GET
/flights/#FlightId/edit FlightEditR GET POST
/static StaticR Static volareStatic
|]


instance Yesod Volare where
    logLevel _ = LevelDebug

    makeSessionBackend _ = do
      key <- getKey "config/client_session_key.aes"
      return $ Just $ clientSessionBackend key 120


instance YesodPersist Volare where
    type YesodPersistBackend Volare = SqlPersist

    runDB action = do
      pool <- volareConnectionPool <$> getYesod
      runSqlPool action pool


instance RenderMessage Volare FormMessage where
    renderMessage _ _ = defaultFormMessage


getRootR :: Handler RepHtml
getRootR = defaultLayout $
           do setTitle "Volare"
              $(whamletFile "templates/root.hamlet")


data NewFlight = NewFlight FileInfo


newFlightAForm :: AForm Volare Volare NewFlight
newFlightAForm = NewFlight <$> fileAFormReq "File"


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
    FormSuccess (NewFlight file) ->
        let handler :: ParseError ->
                       Handler RepHtml
            handler e = do
                 $(logDebug) $ T.pack $ show e
                 listFlights flightWidget enctype
        in handle handler $ do
                 let name = fileName file
                 igc <- liftIO $ runResourceT $ fileSource file $$ sinkParser IGC.igc
                 $(logDebug) $ T.pack $ show igc
                 flightId <- runDB $ do
                               flightId <- insert $ Flight name (IGC.date igc)
                               forM_ (zip (IGC.records igc) [1..]) $ \(record, index) -> do
                                       let position = IGC.position record
                                       insert $ Record flightId
                                                       index
                                                       (UTCTime (IGC.date igc) (IGC.time record))
                                                       (realToFrac $ IGC.latitude position)
                                                       (realToFrac $ IGC.longitude position)
                                                       (realToFrac $ IGC.altitude position)
                               return flightId
                 redirect $ FlightR flightId
    _ -> listFlights flightWidget enctype


listFlights :: Widget ->
               Enctype ->
               Handler RepHtml
listFlights flightWidget enctype = do
  flights <- runDB $ selectList [] []
  defaultLayout $ do
    setTitle "Flights - Volare"
    $(whamletFile "templates/flights/index.hamlet")


data ShowFlight = ShowFlight FlightId Flight [Record]

instance JSON.ToJSON ShowFlight where
    toJSON (ShowFlight id flight records) =
        JSON.object [
                 "id" .= id,
                 "name" .= flightName flight,
                 "records" .= records
                ]


getFlightR :: FlightId ->
              Handler RepHtmlJson
getFlightR flightId = do
  flight <- runDB $ get404 flightId
  records <- runDB $ selectList [RecordFlightId ==. flightId] [Asc RecordIndex]
  googleApiKey <- (Config.googleApiKey . volareConfig) <$> getYesod
  let html = do
        setTitle "Flight - Volare"
        addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/1.8.1/jquery.min.js"
        addScriptRemote $ "http://maps.googleapis.com/maps/api/js?key=" <> googleApiKey <> "&sensor=false"
        addScript $ StaticR S.js_underscore_min_js
        addScript $ StaticR S.js_underscore_string_min_js
        addScript $ StaticR S.js_flight_js
        addScript $ StaticR S.js_volare_js
        addStylesheet $ StaticR S.css_flight_css
        $(whamletFile "templates/flights/show.hamlet")
      json = ShowFlight flightId flight $ map (\(Entity _ r) -> r) records
  defaultLayoutJson html json


data EditFlight = EditFlight T.Text


editFlightAForm :: Maybe Flight ->
                   AForm Volare Volare EditFlight
editFlightAForm flight = EditFlight <$> areq textField "Name" (flightName <$> flight)


editFlightForm :: Maybe Flight ->
                  Html ->
                  MForm Volare Volare (FormResult EditFlight, Widget)
editFlightForm = renderDivs . editFlightAForm


getFlightEditR :: FlightId ->
                  Handler RepHtml
getFlightEditR flightId = do
  flight <- runDB $ get404 flightId
  (flightWidget, enctype) <- generateFormPost $ editFlightForm $ Just flight
  editFlight flightId flightWidget enctype


postFlightEditR :: FlightId ->
                   Handler RepHtml
postFlightEditR flightId = do
  ((result, flightWidget), enctype) <- runFormPost $ editFlightForm Nothing
  case result of
    FormSuccess (EditFlight name) -> do
                runDB $ update flightId [FlightName =. name]
                redirect $ FlightR flightId
    _ -> editFlight flightId flightWidget enctype


editFlight :: FlightId ->
              Widget ->
              Enctype ->
              Handler RepHtml
editFlight flightId flightWidget enctype =
    defaultLayout $ do
      setTitle "Edit Flight - Volare"
      $(whamletFile "templates/flights/edit.hamlet")


formatPosition :: Double ->
                  String
formatPosition = printf "%.5f"


formatLatitude :: Double ->
                  String
formatLatitude = printf "%.0f"


main :: IO ()
main = do
  config <- loadConfig "config/config.yml"
  withSqlitePool (Config.sqlitePath config) (Config.sqliteConnectionPoolCount config) $ \pool -> do
         runSqlPool (runMigration migrateAll) pool
         s <- staticSite
         warpDebug 3000 $ Volare config pool s
