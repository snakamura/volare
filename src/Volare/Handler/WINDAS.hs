module Volare.Handler.WINDAS
    ( getWINDASR
    ) where

import Control.Arrow ((&&&))
import Control.Exception
      ( IOException
      , catch
      )
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson ((.=))
import qualified Data.Aeson as JSON
import Data.Function (on)
import Data.Functor ((<$>))
import Data.List (groupBy)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Formatting ((%))
import qualified Formatting as F
import Pipes ((>->))
import qualified Pipes as P
import qualified Pipes.ByteString as PB
import qualified Pipes.Prelude as P
import qualified Service.WINDAS as WINDAS
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


getWINDASR :: Int ->
              Int ->
              Int ->
              Int ->
              Handler JSON.Value
getWINDASR year month day hour = do
    nwLatitude <- (>>= readMaybe . T.unpack) <$> lookupGetParam "nwlat"
    nwLongitude <- (>>= readMaybe . T.unpack) <$> lookupGetParam "nwlng"
    seLatitude <- (>>= readMaybe . T.unpack) <$> lookupGetParam "selat"
    seLongitude <- (>>= readMaybe . T.unpack) <$> lookupGetParam "selng"
    case (nwLatitude, nwLongitude, seLatitude, seLongitude) of
        (Just nwLat, Just nwLng, Just seLat, Just seLng) -> do
            let stations = WINDAS.stations (nwLat, nwLng) (seLat, seLng)
            liftIO $ JSON.toJSON <$> loadItems stations year month day hour
        _ -> notFound


loadItems :: [WINDAS.Station] ->
             Int ->
             Int ->
             Int ->
             Int ->
             IO [Item]
loadItems stations year month day hour = do
    let path = TL.unpack $ F.format ("./data/windas/" % F.left 4 '0' % "/" % F.left 2 '0' % "/" % F.left 2 '0' % "/" % F.left 2 '0' % ".tar.gz") year month day hour
    b <- doesFileExist path
    unless b $ do
        createDirectoryIfMissing True $ takeDirectory path
        WINDAS.downloadArchive year month day hour $ \producer ->
             withSystemTempFile "windas.tar.gz" $ \tempPath handle -> do
                 P.runEffect $ producer >-> PB.toHandle handle
                 hClose handle
                 renameFile tempPath path `catch` \(_ :: IOException) -> return ()
    observations <- withFile path ReadMode $ \handle ->
        P.toListM $ WINDAS.parseStations stations (PB.fromHandle handle)
    return $ map (uncurry Item . (fst . head &&& map snd)) $ groupBy ((==) `on` fst) observations


data Item = Item WINDAS.Station [WINDAS.Observation]

instance JSON.ToJSON Item where
    toJSON (Item station observations) =
        JSON.object [ "station" .= JSON.object [ "id"        .= WINDAS.id station
                                               , "latitude"  .= WINDAS.latitude station
                                               , "longitude" .= WINDAS.longitude station
                                               , "name"      .= WINDAS.name station
                                               ]
                    , "observations" .= map O observations
                    ]


newtype O = O WINDAS.Observation

instance JSON.ToJSON O where
    toJSON (O o) =
        JSON.object [ "year"   .= WINDAS.year o
                    , "month"  .= WINDAS.month o
                    , "day"    .= WINDAS.day o
                    , "hour"   .= WINDAS.hour o
                    , "minute" .= WINDAS.minute o
                    , "items"  .= map I (WINDAS.items o)
                    ]


newtype I = I WINDAS.Item

instance JSON.ToJSON I where
    toJSON (I i) =
        JSON.object [ "altitude"      .= WINDAS.altitude i
                    , "eastwardWind"  .= WINDAS.eastwardWind i
                    , "northwardWind" .= WINDAS.northwardWind i
                    , "verticalWind"  .= WINDAS.verticalWind i
                    ]
