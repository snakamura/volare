module Volare.Handler.Flight (
    getFlightsR,
    postFlightsR,
    getFlightR,
    getFlightEditR,
    postFlightEditR
) where

import qualified Codec.IGC as IGC
import Control.Applicative ((<$>))
import Control.Exception.Lifted (handle)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson ((.=))
import qualified Data.Aeson as JSON
import Data.Conduit (($$),
                     runResourceT)
import Data.Conduit.Attoparsec (ParseError,
                                sinkParser)
import Data.Foldable (forM_)
import Data.List (maximumBy,
                  minimumBy)
import Data.Monoid ((<>))
import Data.Ord (comparing)
import qualified Data.Text as T
import Data.Time (UTCTime(UTCTime),
                  formatTime)
import Database.Persist (Entity(Entity),
                         Key,
                         PersistStore,
                         SelectOpt(Asc),
                         (=.),
                         (==.),
                         insert,
                         update,
                         selectList)
import System.Locale (defaultTimeLocale)
import Text.Printf (printf)
import Yesod.Core (defaultLayout,
                   logDebug)
import Yesod.Content (RepHtml,
                      RepHtmlJson)
import Yesod.Form (Enctype,
                   FormResult(FormSuccess),
                   areq,
                   fileAFormReq,
                   generateFormPost,
                   renderDivs,
                   runFormPost,
                   textField)
import Yesod.Handler (redirect)
import Yesod.Json (defaultLayoutJson)
import Yesod.Persist (get404,
                      runDB)
import Yesod.Request (FileInfo,
                      fileName,
                      fileSource)
import Yesod.Widget (addScript,
                     addScriptRemote,
                     addStylesheet,
                     addStylesheetRemote,
                     setTitle)

import qualified Volare.Config as Config
import Volare.Foundation
import qualified Volare.Model as M
import Volare.Settings (widgetFile)
import qualified Volare.Static as S


data NewFlight = NewFlight FileInfo


newFlightForm :: Form NewFlight
newFlightForm = renderDivs $ NewFlight <$> fileAFormReq "File"


getFlightsR :: Handler RepHtmlJson
getFlightsR = do
  flights <- runDB $ selectList [] []
  (flightWidget, enctype) <- generateFormPost $ newFlightForm
  let html = flightsWidget flights flightWidget enctype
      json = flights
  defaultLayoutJson html json


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
                 igc <- liftIO $ runResourceT $ fileSource file $$ sinkParser IGC.igc
                 $(logDebug) $ T.pack $ show igc
                 flightId <- runDB $ addFlight (fileName file) igc
                 redirect $ FlightR flightId
    _ -> listFlights flightWidget enctype


listFlights :: Widget ->
               Enctype ->
               Handler RepHtml
listFlights flightWidget enctype = do
  flights <- runDB $ selectList [] []
  defaultLayout $ flightsWidget flights flightWidget enctype


flightsWidget :: [Entity M.Flight] ->
                 Widget ->
                 Enctype ->
                 Widget
flightsWidget flights flightWidget enctype = do
  setTitle "Flights - Volare"
  $(widgetFile "flights/index")


data Flight = Flight M.FlightId M.Flight [Entity M.Record]

instance JSON.ToJSON Flight where
    toJSON (Flight id flight records) =
        JSON.object [
                 "id" .= id,
                 "name" .= M.flightName flight,
                 "minLatitude" .= M.flightMinLatitude flight,
                 "maxLatitude" .= M.flightMaxLatitude flight,
                 "minLongitude" .= M.flightMinLongitude flight,
                 "maxLongitude" .= M.flightMaxLongitude flight,
                 "minAltitude" .= M.flightMinAltitude flight,
                 "maxAltitude" .= M.flightMaxAltitude flight,
                 "records" .= records
                ]


getFlightR :: M.FlightId ->
              Handler RepHtmlJson
getFlightR flightId = do
  flight <- runDB $ get404 flightId
  records <- runDB $ selectList [M.RecordFlightId ==. flightId] [Asc M.RecordIndex]
  googleApiKey <- Config.googleApiKey <$> getConfig
  let html = do
        setTitle "Flight - Volare"
        addScriptRemote "//ajax.googleapis.com/ajax/libs/jquery/1.8.1/jquery.min.js"
        addScriptRemote "//ajax.googleapis.com/ajax/libs/jqueryui/1.8.23/jquery-ui.min.js"
        addScriptRemote $ "//maps.googleapis.com/maps/api/js?key=" <> googleApiKey <> "&sensor=false"
        addStylesheetRemote $ "http://ajax.googleapis.com/ajax/libs/jqueryui/1.8.23/themes/ui-lightness/jquery-ui.css"
        addScript $ StaticR S.js_underscore_min_js
        addScript $ StaticR S.js_underscore_string_min_js
        addScript $ StaticR S.js_flight_js
        addScript $ StaticR S.js_volare_js
        addStylesheet $ StaticR S.css_common_css
        addStylesheet $ StaticR S.css_volare_css
        addStylesheet $ StaticR S.css_flight_css
        $(widgetFile "flights/show")
      json = Flight flightId flight records
  defaultLayoutJson html json


data EditFlight = EditFlight T.Text


editFlightForm :: Maybe M.Flight ->
                  Form EditFlight
editFlightForm flight = renderDivs $ EditFlight <$> areq textField "Name" (M.flightName <$> flight)


getFlightEditR :: M.FlightId ->
                  Handler RepHtml
getFlightEditR flightId = do
  flight <- runDB $ get404 flightId
  (flightWidget, enctype) <- generateFormPost $ editFlightForm $ Just flight
  editFlight flightId flightWidget enctype


postFlightEditR :: M.FlightId ->
                   Handler RepHtml
postFlightEditR flightId = do
  ((result, flightWidget), enctype) <- runFormPost $ editFlightForm Nothing
  case result of
    FormSuccess (EditFlight name) -> do
                runDB $ update flightId [M.FlightName =. name]
                redirect $ FlightR flightId
    _ -> editFlight flightId flightWidget enctype


editFlight :: M.FlightId ->
              Widget ->
              Enctype ->
              Handler RepHtml
editFlight flightId flightWidget enctype =
    defaultLayout $ do
      setTitle "Edit Flight - Volare"
      $(widgetFile "flights/edit")


addFlight :: PersistStore backend m =>
             T.Text ->
             IGC.IGC ->
             backend m (Key backend (M.FlightGeneric backend))
addFlight name igc =
    let records = IGC.records igc
        v c p = realToFrac $ p $ IGC.position $ c (comparing (p . IGC.position)) records
    in do
      flightId <- insert $ M.Flight name
                                    (IGC.date igc)
                                    (v minimumBy IGC.latitude)
                                    (v maximumBy IGC.latitude)
                                    (v minimumBy IGC.longitude)
                                    (v maximumBy IGC.longitude)
                                    (v minimumBy IGC.altitude)
                                    (v maximumBy IGC.altitude)
      forM_ (zip records [1..]) $ \(record, index) -> do
         let position = IGC.position record
         insert $ M.Record flightId
                           index
                           (UTCTime (IGC.date igc) (IGC.time record))
                           (realToFrac $ IGC.latitude position)
                           (realToFrac $ IGC.longitude position)
                           (realToFrac $ IGC.altitude position)
      return flightId


formatPosition :: Double ->
                  String
formatPosition = printf "%.5f"


formatLatitude :: Double ->
                  String
formatLatitude = printf "%.0f"
