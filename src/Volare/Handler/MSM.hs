module Volare.Handler.MSM
    ( getSurfaceR
    , getBarometricR
    ) where

import Control.Exception
    ( IOException
    , catch
    )
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as JSON
import Data.Functor ((<$>))
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Formatting ((%))
import qualified Formatting as F
import qualified Pipes.ByteString as PB
import qualified Service.MSM as MSM
import System.Directory
    ( createDirectoryIfMissing
    , doesFileExist
    , renameFile
    )
import System.FilePath (takeDirectory)
import System.IO (hClose)
import System.IO.Temp (withSystemTempFile)
import Text.Read (readMaybe)
import Yesod.Core.Handler
    ( lookupGetParam
    , notFound
    )

import Volare.Foundation


getSurfaceR :: Int ->
               Int ->
               Int ->
               Int ->
               Handler JSON.Value
getSurfaceR = getData True MSM.getSurfaceItems


getBarometricR :: Int ->
                  Int ->
                  Int ->
                  Int ->
                  Handler JSON.Value
getBarometricR = getData False MSM.getBarometricItems


getData :: JSON.ToJSON a =>
           Bool ->
           (FilePath -> (Float, Float) -> (Float, Float) -> Int -> IO a) ->
           Int ->
           Int ->
           Int ->
           Int ->
           Handler JSON.Value
getData surface f year month day hour = do
    nwLatitude <- (>>= readMaybe . T.unpack) <$> lookupGetParam "nwlat"
    nwLongitude <- (>>= readMaybe . T.unpack) <$> lookupGetParam "nwlng"
    seLatitude <- (>>= readMaybe . T.unpack) <$> lookupGetParam "selat"
    seLongitude <- (>>= readMaybe . T.unpack) <$> lookupGetParam "selng"
    case (nwLatitude, nwLongitude, seLatitude, seLongitude) of
        (Just nwLat, Just nwLng, Just seLat, Just seLng) -> do
            path <- liftIO $ dataFile surface year month day
            liftIO $ JSON.toJSON <$> f path (nwLat, nwLng) (seLat, seLng) hour
        _ -> notFound


dataFile :: Bool ->
            Int ->
            Int ->
            Int ->
            IO FilePath
dataFile surface year month day = do
    let t = if surface then 's' else 'p'
        path = TL.unpack $ F.format ("./data/msm/" % F.char % "/" % F.left 4 '0' % F.left 2 '0' % F.left 2 '0' % ".nc") t year month day
    b <- doesFileExist path
    unless b $ do
        createDirectoryIfMissing True $ takeDirectory path
        withSystemTempFile "msm.nc" $ \tempPath handle -> do
            MSM.download surface year month day $ PB.toHandle handle
            hClose handle
            renameFile tempPath path `catch` \(_ :: IOException) -> return ()
    return path
