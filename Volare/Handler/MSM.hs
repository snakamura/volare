module Volare.Handler.MSM (
    getMSMR
) where

import Control.Applicative ((<$>))
import Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as JSON
import Text.Read (readMaybe)
import qualified Data.Text as T
import Yesod.Core.Handler (lookupGetParam,
                           notFound)

import Volare.Foundation
import qualified Volare.MSM as MSM


getMSMR :: Int ->
           Int ->
           Int ->
           Int ->
           Handler JSON.Value
getMSMR _year _month _day hour = do
  nwLatitude <- (>>= readMaybe . T.unpack) <$> lookupGetParam "nwlat"
  nwLongitude <- (>>= readMaybe . T.unpack) <$> lookupGetParam "nwlon"
  seLatitude <- (>>= readMaybe . T.unpack) <$> lookupGetParam "selat"
  seLongitude <- (>>= readMaybe . T.unpack) <$> lookupGetParam "selon"
  case (nwLatitude, nwLongitude, seLatitude, seLongitude) of
    (Just nwLat, Just nwLon, Just seLat, Just seLon) -> do
      surfaces <- liftIO $ MSM.getSurface "/Users/snakamura/Desktop/netcdf/s0504.nc" (nwLat, nwLon) (seLat, seLon) hour
      return $ JSON.toJSON surfaces
    _ -> notFound
