module MSMSpec (spec) where

import Control.Applicative ((<$>))
import Crypto.Hash.SHA1 (hashlazy)
import qualified Data.ByteString.Lazy as BL
import Pipes
    ( (>->)
    , runEffect
    )
import qualified Pipes.ByteString as PB
import System.IO (hClose)
import System.IO.Temp (withSystemTempFile)
import Test.Hspec
import Text.Bytedump (dumpRawBS)

import qualified Service.MSM as MSM
import qualified Service.MSM.Surface as Surface
import qualified Service.MSM.Barometric as Barometric

import SpecUtils


spec :: Spec
spec = do
    describe "download" $ do
        it "downloads surface data" $ do
            withSystemTempFile "msmspec_s.nc" $ \path handle -> do
                MSM.download True 2014 4 24 $ \producer ->
                    runEffect $ producer >-> PB.toHandle handle
                hClose handle
                hash <- hashlazy <$> BL.readFile path
                dumpRawBS hash @== "8027ea10a8d752a4308fc87fca3fb198d29eded5"

                items <- MSM.getSurfaceItems path (37, 140) (36, 141) 3
                length items @== 374
                let item = head items
                Surface.latitude item @== 37.05
                Surface.longitude item @== 140.0
                Surface.airPressure item @== 1023.2
                Surface.surfaceAirPressure item @== 956.2
                Surface.eastwardWind item @== -0.96
                Surface.northwardWind item @== 1.46
                Surface.airTemperature item @== 14.0
                Surface.relativeHumidity item @== 31
                Surface.rainfallRate item @== 0.0
                Surface.upperCloudiness item @== 0
                Surface.midCloudiness item @== 3
                Surface.lowCloudiness item @== 0
                Surface.cloudAmount item @== 3

        it "downloads barometric data" $ do
            withSystemTempFile "msmspec_p.nc" $ \path handle -> do
                MSM.download False 2014 3 9 $ \producer ->
                    runEffect $ producer >-> PB.toHandle handle
                hClose handle
                hash <- hashlazy <$> BL.readFile path
                dumpRawBS hash @== "6202e74ef30fb541da50c8fbb13095c0f79e2383"

                items <- MSM.getBarometricItems path (37.5, 139.5) (36.3, 140.2) 5
                length items @== 1568
                let item = head items
                Barometric.latitude item @== 37.6
                Barometric.longitude item @== 139.5
                Barometric.airPressure item @== 1000
                Barometric.geopotentialHeight item @== 101.0
                Barometric.lagrangianTendencyOfAirPressure item @== 0.0
                Barometric.eastwardWind item @== 2.51
                Barometric.northwardWind item @== -1.17
                Barometric.airTemperature item @== -0.8
                Barometric.relativeHumidity item @== 85
