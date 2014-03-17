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
                         delete,
                         deleteWhere,
                         insert,
                         update,
                         selectFirst,
                         selectList)
import Text.Printf (printf)
import Yesod.Core (defaultLayout)
import Yesod.Core.Handler (addHeader,
                           invalidArgs,
                           provideRep,
                           selectRep)
import Yesod.Core.Json (requireJsonBody)
import Yesod.Core.Types (TypedContent)
import Yesod.Core.Widget (addScript,
                          addScriptRemote,
                          addStylesheet,
                          setTitle)
import Yesod.Persist (get404,
                      runDB)

import qualified Volare.Config as Config
import Volare.Foundation
import Volare.Handler.Utils (addCommonLibraries)
import qualified Volare.Model as M
import Volare.Settings (widgetFile)
import qualified Volare.Static as S


getFlightsR :: Handler TypedContent
getFlightsR = do
    addHeader "Vary" "Accept"
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
    records <- runDB $ selectList [M.RecordFlightId ==. flightId] [Asc M.RecordIndex]
    googleApiKey <- Config.googleApiKey <$> getConfig
    addHeader "Vary" "Accept"
    selectRep $ do
        provideRep $ defaultLayout $ do
            setTitle "Flight - Volare"
            addCommonLibraries
            addScriptRemote $ "//maps.googleapis.com/maps/api/js?key=" <> googleApiKey <> "&sensor=false"
            addScript $ StaticR S.js_common_js
            addScript $ StaticR S.js_volare_js
            addScript $ StaticR S.js_flight_js
            addStylesheet $ StaticR S.css_common_css
            addStylesheet $ StaticR S.css_volare_css
            addStylesheet $ StaticR S.css_flight_css
            let weather = $(widgetFile "elements/weather")
            $(widgetFile "flights/show")
        provideRep $ return $ JSON.toJSON $ Flight flightId flight records


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
    runDB $ do
        deleteWhere [M.RecordFlightId ==. flightId]
        deleteWhere [M.WorkspaceFlightFlightId ==. flightId]
        delete flightId
    return $ JSON.toJSON ()


addFlight :: (PersistStore m, PersistMonadBackend m ~ PersistEntityBackend M.Flight) =>
             T.Text ->
             IGC.IGC ->
             m M.FlightId
addFlight name igc = do
    let records = IGC.records igc
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


formatPosition :: Double ->
                  String
formatPosition = printf "%.5f"


formatLatitude :: Double ->
                  String
formatLatitude = printf "%.0f"
