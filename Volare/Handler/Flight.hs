module Volare.Handler.Flight (
    getFlightsR,
    postFlightsR,
    getFlightR,
    putFlightR,
    deleteFlightR
) where

import qualified Codec.IGC as IGC
import Control.Applicative ((<$>),
                            (<*>))
import Control.Monad (filterM,
                      when)
import Control.Monad.State (evalState,
                            get,
                            put)
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
import qualified Data.Text.Encoding as T
import Data.Time (UTCTime(UTCTime))
import Database.Persist (Entity,
                         PersistEntityBackend,
                         PersistMonadBackend,
                         PersistStore,
                         SelectOpt(Asc, Desc),
                         (=.),
                         (==.),
                         deleteCascade,
                         insert,
                         update,
                         selectFirst,
                         selectList)
import Text.Blaze.Html (toHtml)
import Text.Printf (printf)
import Yesod.Core (defaultLayout)
import Yesod.Core.Handler (invalidArgs,
                           provideRep,
                           selectRep)
import Yesod.Core.Json (requireJsonBody)
import Yesod.Core.Types (TypedContent)
import Yesod.Core.Widget (addScript,
                          addStylesheet,
                          setTitle)
import Yesod.Persist (get404,
                      runDB)

import Volare.Foundation
import Volare.Handler.Utils (addCommonLibraries,
                             addGoogleMapsApi)
import qualified Volare.Model as M
import Volare.Settings (widgetFile)
import qualified Volare.Static as S


getFlightsR :: Handler TypedContent
getFlightsR = do
    selectRep $ do
        provideRep $ defaultLayout $ do
            setTitle "Flights - Volare"
            addCommonLibraries
            addScript $ StaticR S.js_common_js
            addScript $ StaticR S.js_flights_js
            addStylesheet $ StaticR S.css_common_css
            addStylesheet $ StaticR S.css_flights_css
            $(widgetFile "flights/index")
        provideRep $ runDB $ JSON.toJSON <$> selectList [] [Desc M.FlightTime]


data NewFlight = NewFlight T.Text B.ByteString

instance JSON.FromJSON NewFlight where
    parseJSON (JSON.Object o) = NewFlight <$> o .: "name"
                                          <*> (T.encodeUtf8 <$> (o .: "igc"))
    parseJSON _ = mempty


postFlightsR :: Handler JSON.Value
postFlightsR = do
    NewFlight name igcBytes <- requireJsonBody
    case parseOnly IGC.igc igcBytes of
      Left _ -> invalidArgs ["igc"]
      Right igc -> do
          flight <- runDB $ do
              flightId <- addFlight name igc
              selectFirst [M.FlightId ==. flightId] []
          return $ JSON.toJSON flight


data Flight = Flight M.FlightId M.Flight [Entity M.Record]

instance JSON.ToJSON Flight where
    toJSON (Flight flightId flight records) =
        JSON.object [
            "id" .= flightId,
            "name" .= M.flightName flight,
            "time" .= M.flightTime flight,
            "duration" .= M.flightDuration flight,
            "minLatitude" .= M.flightMinLatitude flight,
            "maxLatitude" .= M.flightMaxLatitude flight,
            "minLongitude" .= M.flightMinLongitude flight,
            "maxLongitude" .= M.flightMaxLongitude flight,
            "minAltitude" .= M.flightMinAltitude flight,
            "maxAltitude" .= M.flightMaxAltitude flight,
            "records" .= records
          ]


getFlightR :: M.FlightId ->
              Handler TypedContent
getFlightR flightId = do
    flight <- runDB $ get404 flightId
    selectRep $ do
        provideRep $ defaultLayout $ do
            setTitle $ toHtml $ M.flightName flight <> " - Flight - Volare"
            addCommonLibraries
            addGoogleMapsApi
            addScript $ StaticR S.js_common_js
            addScript $ StaticR S.js_volare_js
            addScript $ StaticR S.js_flight_js
            addStylesheet $ StaticR S.css_common_css
            addStylesheet $ StaticR S.css_volare_css
            addStylesheet $ StaticR S.css_flight_css
            let options = $(widgetFile "elements/options")
                weather = $(widgetFile "elements/weather")
            $(widgetFile "flights/show")
        provideRep $ do
            records <- runDB $ selectList [M.RecordFlightId ==. flightId] [Asc M.RecordIndex]
            return $ JSON.toJSON $ Flight flightId flight records


data EditFlight = EditFlight T.Text

instance JSON.FromJSON EditFlight where
    parseJSON (JSON.Object o) = EditFlight <$> o .: "name"
    parseJSON _ = mempty


putFlightR :: M.FlightId ->
              Handler JSON.Value
putFlightR flightId = do
    EditFlight name <- requireJsonBody
    flight <- runDB $ do
        update flightId [M.FlightName =. name]
        selectFirst [M.FlightId ==. flightId] []
    return $ JSON.toJSON flight


deleteFlightR :: M.FlightId ->
                 Handler JSON.Value
deleteFlightR flightId = do
    runDB $ deleteCascade flightId
    return $ JSON.toJSON ()


addFlight :: (PersistStore m, PersistMonadBackend m ~ PersistEntityBackend M.Flight) =>
             T.Text ->
             IGC.IGC ->
             m M.FlightId
addFlight name igc = do
    let records = filterRecords $ IGC.records igc
        value selector property = realToFrac $ property $ IGC.position $ selector (comparing (property . IGC.position)) records
    flightId <- insert $ M.Flight name
                                  (UTCTime (IGC.date igc) (IGC.time $ head records))
                                  (round ((IGC.time $ last records) - (IGC.time $ head records)))
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
    where
      filterRecords records =
          case reverse $ dropWhileNotFlying $ reverse $ dropWhileNotFlying records of
            [] -> records
            body -> let start = IGC.time $ head body
                        pre record = IGC.time record < start - 60
                        end = IGC.time $ last body
                        post record = IGC.time record > end + 60
                    in flip evalState Nothing $ filterM valid $ takeWhile (not . post) $ dropWhile pre records
      dropWhileNotFlying records = map fst $ dropWhile (not . uncurry flying) $ zip records (drop 10 records)
      flying record next = let duration = abs $ IGC.time next - IGC.time record
                               dist = distance (IGC.position next) (IGC.position record)
                               speed = dist / realToFrac duration
                           in speed > 5 && speed < 10
      valid record = do
          previousRecord <- get
          let altitude = IGC.altitude $ IGC.position record
              time = IGC.time record
              v = maybe True (\p -> (abs (IGC.altitude (IGC.position p) - altitude)) / realToFrac (time - IGC.time p) < 100) previousRecord
          when v $
              put $ Just record
          return v


distance :: IGC.Position ->
            IGC.Position ->
            Double
distance position1 position2 =
    let r = 6378137
        dx = (realToFrac (IGC.longitude position1) - realToFrac (IGC.longitude position2)) / 180 * pi
        y1 = realToFrac (IGC.latitude position1) / 180 * pi
        y2 = realToFrac (IGC.latitude position2) / 180 * pi
    in r * acos (sin y1 * sin y2 + cos y1 * cos y2 * cos dx)


formatPosition :: Double ->
                  String
formatPosition = printf "%.5f"


formatLatitude :: Double ->
                  String
formatLatitude = printf "%.0f"
