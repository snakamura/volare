module Volare.Handler.UAS
    ( getUASR
    , getUASStationsR
    ) where

import Control.Exception
      ( IOException
      , catch
      )
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.State.Strict (evalStateT)
import Data.Aeson ((.=))
import qualified Data.Aeson as JSON
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Formatting ((%))
import qualified Formatting as F
import qualified Network.HTTP.Client as Http
import Pipes ((>->))
import qualified Pipes as P
import qualified Pipes.ByteString as PB
import qualified Service.UAS as UAS
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
import Yesod.Core (defaultLayout)
import Yesod.Core.Handler
    ( lookupGetParam
    , notFound
    , provideRep
    , selectRep
    )
import Yesod.Core.Types (TypedContent)
import Yesod.Core.Widget
    ( addStylesheet
    , setTitle
    )

import Volare.Foundation
import Volare.Handler.Utils
    ( addCommonLibraries
    , addRequireJS
    )
import Volare.Settings (widgetFile)
import qualified Volare.Static as S
import qualified Volare.Widget as W


getUASR :: Int ->
           Int ->
           Int ->
           Int ->
           Int ->
           Handler TypedContent
getUASR stationId year month day hour = do
    case UAS.station stationId of
        Just station -> selectRep $ do
            provideRep $ defaultLayout $ do
                setTitle "Upper Air Sounding - Volare"
                addCommonLibraries
                addRequireJS "uas/show"
                addStylesheet $ StaticR S.css_common_css
                $(widgetFile "uas/show")
            provideRep $ do
                manager <- getHttpManager
                liftIO (load station year month day hour manager) >>= \case
                    Just items -> return $ JSON.toJSON items
                    Nothing -> notFound
        Nothing -> notFound


getUASStationsR :: Handler TypedContent
getUASStationsR = do
    nwLatitude <- (>>= readMaybe . T.unpack) <$> lookupGetParam "nwlat"
    nwLongitude <- (>>= readMaybe . T.unpack) <$> lookupGetParam "nwlng"
    seLatitude <- (>>= readMaybe . T.unpack) <$> lookupGetParam "selat"
    seLongitude <- (>>= readMaybe . T.unpack) <$> lookupGetParam "selng"
    let stations = case (nwLatitude, nwLongitude, seLatitude, seLongitude) of
                       (Just nwLat, Just nwLng, Just seLat, Just seLng) -> UAS.stations (nwLat, nwLng) (seLat, seLng)
                       _ -> UAS.allStations
    selectRep $ do
        provideRep $ defaultLayout $ do
            setTitle "Upper Air Sounding - Volare"
            addCommonLibraries
            addRequireJS "uas/index"
            addStylesheet $ StaticR S.css_common_css
            $(widgetFile "uas/index")
        provideRep $ return $ JSON.toJSON $ map S stations


load :: UAS.Station ->
        Int ->
        Int ->
        Int ->
        Int ->
        Http.Manager ->
        IO (Maybe [Item])
load station year month day hour manager = do
    let path = TL.unpack $ F.format ("./data/uas/" % F.int % "/" % F.left 4 '0' % "/" % F.left 2 '0' % "/" % F.left 2 '0' % "/" % F.left 2 '0') (UAS.id station) year month day hour
    b <- doesFileExist path
    unless b $ do
        createDirectoryIfMissing True $ takeDirectory path
        UAS.download station year month day hour manager $ \producer ->
             withSystemTempFile "uas" $ \tempPath handle -> do
                 P.runEffect $ producer >-> PB.toHandle handle
                 hClose handle
                 renameFile tempPath path `catch` \(_ :: IOException) -> return ()
    observation <- withFile path ReadMode $ evalStateT UAS.parser . PB.fromHandle
    return $ (map (Item station) . UAS.items) <$> observation


newtype S = S UAS.Station

instance JSON.ToJSON S where
    toJSON (S s) =
        JSON.object [ "id"        .= UAS.id s
                    , "latitude"  .= UAS.latitude s
                    , "longitude" .= UAS.longitude s
                    , "elevation" .= UAS.elevation s
                    , "name"      .= UAS.name s
                    ]


data Item = Item UAS.Station UAS.Item

instance JSON.ToJSON Item where
    toJSON (Item station item) =
        let entry = UAS.entry item
            UAS.Pressure pressure = UAS.pressure item
            height = case UAS.plane item of
                         UAS.Surface -> Just $ UAS.elevation station
                         UAS.Barometric h -> h
        in JSON.object [ "pressure"      .= pressure
                       , "height"        .= height
                       , "temperature"   .= UAS.temperature entry
                       , "dewPoint"      .= UAS.dewPoint entry
                       , "windDirection" .= UAS.windDirection entry
                       , "windSpeed"     .= UAS.windSpeed entry
                       ]
