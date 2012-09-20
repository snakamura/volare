module Volare.Handler.Flight (
    getFlightsR,
    postFlightsR,
    getFlightR,
    getFlightEditR,
    postFlightEditR
) where

import qualified Codec.IGC as IGC
import Control.Applicative ((<$>),
                            (<*>))
import Data.Aeson ((.=),
                   (.:))
import qualified Data.Aeson as JSON
import Data.Attoparsec (parseOnly)
import qualified Data.ByteString as B
import Data.Foldable (forM_)
import Data.List (maximumBy,
                  minimumBy)
import Data.Monoid ((<>),
                    mempty)
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
                         selectFirst,
                         selectList)
import System.Locale (defaultTimeLocale)
import Text.Printf (printf)
import Yesod.Core (defaultLayout,
                   logDebug)
import Yesod.Content (RepHtml,
                      RepHtmlJson,
                      RepJson)
import Yesod.Form (Enctype,
                   FormResult(FormSuccess),
                   areq,
                   generateFormPost,
                   renderDivs,
                   runFormPost,
                   textField)
import Yesod.Handler (invalidArgs,
                      redirect)
import Yesod.Json (defaultLayoutJson,
                   jsonToRepJson,
                   parseJsonBody_)
import Yesod.Persist (get404,
                      runDB)
import Yesod.Widget (addScript,
                     addScriptRemote,
                     addStylesheet,
                     setTitle)

import qualified Volare.Config as Config
import Volare.Foundation
import Volare.Handler.Utils (addCommonLibraries)
import qualified Volare.Model as M
import Volare.Settings (widgetFile)
import qualified Volare.Static as S


getFlightsR :: Handler RepHtmlJson
getFlightsR = do
    flights <- runDB $ selectList [] []
    let html = do
            setTitle "Flights - Volare"
            addCommonLibraries
            addScript $ StaticR S.js_common_js
            addScript $ StaticR S.js_flights_js
            addStylesheet $ StaticR S.css_common_css
            $(widgetFile "flights/index")
        json = flights
    defaultLayoutJson html json


data NewFlight = NewFlight T.Text B.ByteString

instance JSON.FromJSON NewFlight where
    parseJSON (JSON.Object o) = NewFlight <$> o .: "name"
                                          <*> (o .: "igc")
    parseJSON _ = mempty


postFlightsR :: Handler RepJson
postFlightsR = do
    NewFlight name igc <- parseJsonBody_
    case parseOnly IGC.igc igc of
      Left _ -> invalidArgs ["igc"]
      Right igc -> do
          flight <- runDB $ do
              flightId <- addFlight name igc
              selectFirst [M.FlightId ==. flightId] []
          jsonToRepJson flight


data Flight = Flight M.FlightId M.Flight [Entity M.Record]

instance JSON.ToJSON Flight where
    toJSON (Flight id flight records) =
        JSON.object [
            "id" .= id,
            "name" .= M.flightName flight,
            "time" .= M.flightTime flight,
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
            addCommonLibraries
            addScriptRemote $ "//maps.googleapis.com/maps/api/js?key=" <> googleApiKey <> "&sensor=false"
            addScript $ StaticR S.js_common_js
            addScript $ StaticR S.js_volare_js
            addScript $ StaticR S.js_flight_js
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
addFlight name igc = do
    let records = IGC.records igc
        value selector property = realToFrac $ property $ IGC.position $ selector (comparing (property . IGC.position)) records
    flightId <- insert $ M.Flight name
                                  (UTCTime (IGC.date igc) (IGC.time $ head records))
                                  (value minimumBy IGC.latitude)
                                  (value maximumBy IGC.latitude)
                                  (value minimumBy IGC.longitude)
                                  (value maximumBy IGC.longitude)
                                  (value minimumBy IGC.altitude)
                                  (value maximumBy IGC.altitude)
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
