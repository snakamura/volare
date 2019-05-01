module Volare.Handler.MSM
    ( getSurfaceR
    , getBarometricR
    , getLatestSurfaceR
    , getLatestBarometricR
    ) where

import Control.Exception
    ( IOException
    , catch
    , catches
    , throwIO
    )
import qualified Control.Exception as E
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as JSON
import qualified Data.ByteString as B
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Time
    ( UTCTime(UTCTime), utctDay, utctDayTime
    , addUTCTime
    , getCurrentTime
    , secondsToDiffTime
    , toGregorian
    )
import Formatting ((%))
import qualified Formatting as F
import Network.HTTP.Client (HttpException)
import Pipes
    ( Producer
    , (>->)
    , runEffect
    )
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
import Volare.Handler.MSM.Timestamp (Timestamp(Timestamp))


getSurfaceR :: Int ->
               Int ->
               Int ->
               Int ->
               Handler JSON.Value
getSurfaceR year month day = getData (dataFile MSM.Surface year month day) MSM.getSurfaceItems


getBarometricR :: Int ->
                  Int ->
                  Int ->
                  Int ->
                  Handler JSON.Value
getBarometricR year month day = getData (dataFile MSM.Barometric year month day) MSM.getBarometricItems


getLatestSurfaceR :: Int ->
                     Handler JSON.Value
getLatestSurfaceR = getData (latestDataFile MSM.Surface) MSM.getSurfaceItems


getLatestBarometricR :: Int ->
                        Handler JSON.Value
getLatestBarometricR = getData (latestDataFile MSM.Barometric) MSM.getBarometricItems


getData :: JSON.ToJSON a =>
           IO (Timestamp, FilePath) ->
           (FilePath -> (Float, Float) -> (Float, Float) -> Int -> IO a) ->
           Int ->
           Handler JSON.Value
getData prepareDataFile getItems hour = do
    nwLatitude <- (>>= readMaybe . T.unpack) <$> lookupGetParam "nwlat"
    nwLongitude <- (>>= readMaybe . T.unpack) <$> lookupGetParam "nwlng"
    seLatitude <- (>>= readMaybe . T.unpack) <$> lookupGetParam "selat"
    seLongitude <- (>>= readMaybe . T.unpack) <$> lookupGetParam "selng"
    case (nwLatitude, nwLongitude, seLatitude, seLongitude) of
        (Just nwLat, Just nwLng, Just seLat, Just seLng) -> liftIO $ do
            (timestamp, path) <- prepareDataFile
            items <- getItems path (nwLat, nwLng) (seLat, seLng) hour
            return $ JSON.Object $ HashMap.fromList [("timestamp", JSON.toJSON timestamp), ("items", JSON.toJSON items)]
        _ -> notFound


dataFile :: MSM.Family ->
            Int ->
            Int ->
            Int ->
            IO (Timestamp, FilePath)
dataFile family year month day = do
    let path = TL.unpack $ F.format ("./data/msm/" % F.char % "/" % F.left 4 '0' % F.left 2 '0' % F.left 2 '0' % ".nc") (symbol family) year month day
    b <- doesFileExist path
    unless b $ download path $ MSM.download family year month day
    return (Timestamp year month day 0, path)


latestDataFile :: MSM.Family ->
                  IO (Timestamp, FilePath)
latestDataFile family = do
    currentTime <- getCurrentTime
    path <- whileM (candidates currentTime) $ \time ->
        let handlers = [E.Handler $ \(_ :: IOException) -> return Nothing,
                        E.Handler $ \(_ :: HttpException) -> return Nothing]
        in (Just <$> prepare time) `catches` handlers
    maybe (throwIO $ userError "Latest data not found.") return path
  where
    prepare time = do
        let (year, month, day) = toGregorian $ utctDay time
            hour = floor (utctDayTime time) `div` (60 * 60) :: Int
            path = TL.unpack $ F.format ("./data/msm.latest/" % F.char % "/" % F.left 4 '0' % F.left 2 '0' % F.left 2 '0' % F.left 2 '0' % ".nc") (symbol family) year month day hour
        b <- doesFileExist path
        unless b $ download path $ MSM.downloadLatest family (fromInteger year) month day hour
        return (Timestamp (fromInteger year) month day hour, path)
    candidates time = let hour = floor (utctDayTime time) `div` (60 * 60) `div` 3 * 3
                          startTime = UTCTime (utctDay time) (secondsToDiffTime (hour * 60 * 60))
                      in iterate (addUTCTime (-3 * 60 * 60)) startTime


symbol :: MSM.Family ->
          Char
symbol MSM.Surface = 's'
symbol MSM.Barometric = 'p'


download :: FilePath ->
            ((Producer B.ByteString IO () -> IO ()) -> IO ()) ->
            IO ()
download path action = do
    withSystemTempFile "msm.nc" $ \tempPath handle -> do
        action $ \producer -> runEffect (producer >-> PB.toHandle handle)
        hClose handle
        createDirectoryIfMissing True $ takeDirectory path
        renameFile tempPath path `catch` \(_ :: IOException) -> return ()


whileM :: Monad m =>
          [a] ->
          (a -> m (Maybe b)) ->
          m (Maybe b)
whileM [] _ = return Nothing
whileM (a:as) action = action a >>= \case
    Just v -> return $ Just v
    Nothing -> whileM as action
