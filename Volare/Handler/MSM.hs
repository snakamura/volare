module Volare.Handler.MSM (
    getMSMR
) where

import Control.Applicative ((<$>))
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as JSON
import Data.Conduit (($$+-))
import qualified Data.Conduit.Binary as CB
import qualified Data.Text as T
import qualified Network.HTTP.Conduit as Http
import System.Directory (createDirectoryIfMissing,
                         doesFileExist)
import System.FilePath (takeDirectory)
import Text.Printf (printf)
import Text.Read (readMaybe)
import Yesod.Core.Handler (lookupGetParam,
                           notFound)

import Volare.Foundation
import qualified Volare.MSM as MSM


getMSMR :: Int ->
           Int ->
           Int ->
           Int ->
           Handler JSON.Value
getMSMR year month day hour = do
  nwLatitude <- (>>= readMaybe . T.unpack) <$> lookupGetParam "nwlat"
  nwLongitude <- (>>= readMaybe . T.unpack) <$> lookupGetParam "nwlon"
  seLatitude <- (>>= readMaybe . T.unpack) <$> lookupGetParam "selat"
  seLongitude <- (>>= readMaybe . T.unpack) <$> lookupGetParam "selon"
  case (nwLatitude, nwLongitude, seLatitude, seLongitude) of
    (Just nwLat, Just nwLon, Just seLat, Just seLon) -> do
      path <- liftIO $ getMSM year month day
      surfaces <- liftIO $ MSM.getSurface path (nwLat, nwLon) (seLat, seLon) hour
      return $ JSON.toJSON surfaces
    _ -> notFound


getMSM :: Int ->
          Int ->
          Int ->
          IO FilePath
getMSM year month day = do
  let path = printf "./data/msm/s/%04d%02d%02d.nc" year month day
      url = printf "http://database.rish.kyoto-u.ac.jp/arch/jmadata/data/gpv/netcdf/MSM-S/%04d/%02d%02d.nc" year month day
  b <- doesFileExist path
  when (not b) $ do
    createDirectoryIfMissing True $ takeDirectory path
    req <- Http.parseUrl url
    Http.withManager $ \manager -> do
      res <- Http.http req manager
      Http.responseBody res $$+- CB.sinkFile path
  return path
