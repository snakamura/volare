module Volare.Handler.AMEDAS
    ( getAMEDASR
    ) where

import Control.Concurrent.Async (mapConcurrently)
import Control.Exception
    ( IOException
    , catch
    )
import Control.Monad.IO.Class (liftIO)
import Data.Aeson ((.=))
import qualified Data.Aeson as JSON
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Time
    ( UTCTime(..)
    , fromGregorian
    , hoursToTimeZone
    , localDay
    , localTimeOfDay
    , todHour
    , toGregorian
    , utcToLocalTime
    )
import Formatting ((%))
import qualified Formatting as F
import qualified Network.HTTP.Client as Http
import Pipes ((>->))
import qualified Pipes as P
import qualified Pipes.ByteString as PB
import qualified Pipes.Prelude as P
import qualified Service.AMEDAS as AMEDAS
import System.Directory
    ( createDirectoryIfMissing
    , doesFileExist
    , renameFile
    )
import System.FilePath (takeDirectory)
import System.IO
    ( IOMode(ReadMode)
    , hClose
    , withFile
    )
import System.IO.Temp (withSystemTempFile)
import Text.Read (readMaybe)
import Yesod.Core.Handler
    ( lookupGetParam
    , notFound
    )

import Volare.Foundation


getAMEDASR :: Int ->
              Int ->
              Int ->
              Int ->
              Handler JSON.Value
getAMEDASR year month day hour = do
    nwLatitude <- (>>= readMaybe . T.unpack) <$> lookupGetParam "nwlat"
    nwLongitude <- (>>= readMaybe . T.unpack) <$> lookupGetParam "nwlng"
    seLatitude <- (>>= readMaybe . T.unpack) <$> lookupGetParam "selat"
    seLongitude <- (>>= readMaybe . T.unpack) <$> lookupGetParam "selng"
    case (nwLatitude, nwLongitude, seLatitude, seLongitude) of
        (Just nwLat, Just nwLng, Just seLat, Just seLng) -> do
            manager <- getHttpManager
            let utcTime = UTCTime (fromGregorian (fromIntegral year) month day) (fromIntegral $ hour * 60 * 60)
                jst = hoursToTimeZone 9
                jstTime = utcToLocalTime jst utcTime
                (y, m, d) = toGregorian $ localDay jstTime
                h = todHour $ localTimeOfDay jstTime
                f (Item _ _ item) = let t = AMEDAS.time item
                                    in h * 60 < t && t <= (h + 1) * 60
                load station = filter f <$> loadItems station (fromIntegral y) m d manager
            liftIO $ JSON.toJSON . concat <$> mapConcurrently load (AMEDAS.stations (nwLat, nwLng) (seLat, seLng))
        _ -> notFound


loadItems :: AMEDAS.Station ->
             Int ->
             Int ->
             Int ->
             Http.Manager ->
             IO [Item]
loadItems station year month day manager = do
    let path = TL.unpack $ F.format ("./data/amedas/" % F.int % "/" % F.left 4 '0' % "/" % F.left 4 '0' % F.left 2 '0' % F.left 2 '0' % ".csv") (AMEDAS.prec station) (AMEDAS.block station) year month day
    b <- doesFileExist path
    items <- if b then
                 withFile path ReadMode $ P.toListM . AMEDAS.load . PB.fromHandle
             else do
                 createDirectoryIfMissing True $ takeDirectory path
                 items <- P.toListM $ AMEDAS.download station year month day manager
                 withSystemTempFile "amedas.csv" $ \tempPath handle -> do
                     P.runEffect $ P.each items >-> AMEDAS.save (PB.toHandle handle)
                     hClose handle
                     renameFile tempPath path `catch` \(_ :: IOException) -> return ()
                 return items
    return $ map (Item (AMEDAS.latitude station) (AMEDAS.longitude station)) items


data Item = Item Float Float AMEDAS.Item

instance JSON.ToJSON Item where
    toJSON (Item latitude longitude item) =
        let hour = (AMEDAS.time item `div` 60 + (24 - 9)) `mod` 24
            minute = AMEDAS.time item `mod` 60
        in JSON.object [ "time"          .= (hour * 60 + minute)
                       , "latitude"      .= latitude
                       , "longitude"     .= longitude
                       , "precipitation" .= AMEDAS.precipitation item
                       , "temperature"   .= AMEDAS.temperature item
                       , "windSpeed"     .= AMEDAS.windSpeed item
                       , "windDirection" .= AMEDAS.windDirection item
                       , "sunshine"      .= AMEDAS.sunshine item
                       ]
