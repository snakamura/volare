module Volare.Handler.AMEDAS (
    getDataR
) where

import Control.Applicative ((<$>))
import Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as JSON
import qualified Data.Text as T
import Data.Time (UTCTime(..),
                  fromGregorian,
                  hoursToTimeZone,
                  localDay,
                  localTimeOfDay,
                  todHour,
                  toGregorian,
                  utcToLocalTime)
import System.Directory (createDirectoryIfMissing,
                         doesFileExist)
import System.FilePath (takeDirectory)
import Text.Printf (printf)
import Text.Read (readMaybe)
import Yesod.Core.Handler (lookupGetParam,
                           notFound)

import qualified Volare.AMEDAS as AMEDAS
import Volare.Foundation


getDataR :: Int ->
            Int ->
            Int ->
            Int ->
            Handler JSON.Value
getDataR year month day hour = do
  nwLatitude <- (>>= readMaybe . T.unpack) <$> lookupGetParam "nwlat"
  nwLongitude <- (>>= readMaybe . T.unpack) <$> lookupGetParam "nwlng"
  seLatitude <- (>>= readMaybe . T.unpack) <$> lookupGetParam "selat"
  seLongitude <- (>>= readMaybe . T.unpack) <$> lookupGetParam "selng"
  case (nwLatitude, nwLongitude, seLatitude, seLongitude) of
    (Just nwLat, Just nwLng, Just seLat, Just seLng) -> do
      let utcTime = UTCTime (fromGregorian (fromIntegral year) month day) (fromIntegral $ hour * 60 * 60)
          jst = hoursToTimeZone 9
          jstTime = utcToLocalTime jst utcTime
          (y, m, d) = toGregorian $ localDay jstTime
          h = todHour $ localTimeOfDay jstTime
          f item = let t = AMEDAS.time item
                   in h * 60 <= t && t < (h + 1) * 60
          load station = (station, ) . filter f <$> loadItems station (fromIntegral y) m d
      liftIO $ JSON.toJSON <$> mapM load (AMEDAS.stations (nwLat, nwLng) (seLat, seLng))
    _ -> notFound


loadItems :: AMEDAS.Station ->
             Int ->
             Int ->
             Int ->
             IO [AMEDAS.Item]
loadItems station year month day = do
  let path = printf "./data/amedas/%d/%04d/%04d%02d%02d.csv" (AMEDAS.prec station) (AMEDAS.block station) year month day
  b <- doesFileExist path
  if b then
      AMEDAS.load path
  else do
    createDirectoryIfMissing True $ takeDirectory path
    items <- AMEDAS.download station year month day
    -- TODO
    -- Save to a temporary file and move it
    AMEDAS.save path items
    return items
