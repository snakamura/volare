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
import Data.Monoid ((<>),
                    mempty)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Time (diffUTCTime)
import Database.Persist (Entity,
                         entityVal)
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
import Yesod.Persist (runDB)

import qualified Volare.Domain as D
import Volare.Foundation
import Volare.Handler.Utils (addCommonLibraries,
                             addGoogleMapsApi,
                             lookupIntegralGetParam,
                             maybeNotFound)
import qualified Volare.Model as M
import Volare.Settings (widgetFile)
import qualified Volare.Static as S
import qualified Volare.Widget as W
import Volare.Widget.Navigation (Page(FLIGHTS),
                                 navigation)


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
        provideRep $ runDB $ JSON.toJSON <$> D.getFlights


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
              flightId <- D.addFlight name igc
              D.getFlight flightId
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
    maybeNotFound (runDB $ D.getFlight flightId) $ \flightEntity -> do
        let flight = entityVal flightEntity
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
                $(widgetFile "flights/show")
            provideRep $ do
                records <- runDB $ D.getFlightRecords flightId
                interval <- fmap fromInteger <$> lookupIntegralGetParam "interval"
                let recordFilter = case interval of
                                    Just i -> flip evalState Nothing . filterM (valid i)
                                    Nothing -> id
                return $ JSON.toJSON $ Flight flightId flight $ recordFilter records
  where
    valid interval record = do
        previousTime <- get
        let time = M.recordTime $ entityVal record
            v = maybe True ((>= interval) . diffUTCTime time) previousTime
        when v $
            put $ Just time
        return v


data EditFlight = EditFlight T.Text

instance JSON.FromJSON EditFlight where
    parseJSON (JSON.Object o) = EditFlight <$> o .: "name"
    parseJSON _ = mempty


putFlightR :: M.FlightId ->
              Handler JSON.Value
putFlightR flightId = do
    EditFlight name <- requireJsonBody
    flight <- runDB $ do
        D.updateFlight flightId (Just name)
        D.getFlight flightId
    return $ JSON.toJSON flight


deleteFlightR :: M.FlightId ->
                 Handler JSON.Value
deleteFlightR flightId = do
    runDB $ D.deleteFlight flightId
    return $ JSON.toJSON ()


formatPosition :: Double ->
                  String
formatPosition = printf "%.5f"


formatLatitude :: Double ->
                  String
formatLatitude = printf "%.0f"
