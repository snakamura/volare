module MSMSpec (spec) where

import Pipes ((>->))
import qualified Pipes.ByteString as PB
import qualified Pipes.Prelude as P
import System.IO (hClose)
import System.IO.Temp (withSystemTempFile)
import Test.Hspec
    ( Spec
    , describe
    , it
    , shouldBe
    )

import qualified Service.MSM as MSM
import qualified Service.MSM.Surface as Surface
import qualified Service.MSM.Barometric as Barometric

import SpecUtils


spec :: Spec
spec = do
    describe "download" $ do
        it "downloads surface data" $ do
            withSystemTempFile "msmspec_s.nc" $ \path handle -> do
                hash <- MSM.download True 2014 4 24 $ \producer ->
                    sha1 $ producer >-> P.tee (PB.toHandle handle)
                hClose handle
                hash `shouldBe` "8027ea10a8d752a4308fc87fca3fb198d29eded5"

                items <- MSM.getSurfaceItems path (37, 140) (36, 141) 3
                length items `shouldBe` 374
                let item = head items
                Surface.latitude item `shouldBe` 37.05
                Surface.longitude item `shouldBe` 140.0
                Surface.airPressure item `shouldBe` 1023.2
                Surface.surfaceAirPressure item `shouldBe` 956.2
                Surface.eastwardWind item `shouldBe` -0.96
                Surface.northwardWind item `shouldBe` 1.46
                Surface.airTemperature item `shouldBe` 14.0
                Surface.relativeHumidity item `shouldBe` 31
                Surface.rainfallRate item `shouldBe` 0.0
                Surface.upperCloudiness item `shouldBe` 0
                Surface.midCloudiness item `shouldBe` 3
                Surface.lowCloudiness item `shouldBe` 0
                Surface.cloudAmount item `shouldBe` 3

        it "downloads barometric data" $ do
            withSystemTempFile "msmspec_p.nc" $ \path handle -> do
                hash <- MSM.download False 2014 3 9 $ \producer ->
                    sha1 $ producer >-> P.tee (PB.toHandle handle)
                hClose handle
                hash `shouldBe` "6202e74ef30fb541da50c8fbb13095c0f79e2383"

                items <- MSM.getBarometricItems path (37.5, 139.5) (36.3, 140.2) 15
                length items `shouldBe` 1568
                let item = head items
                Barometric.latitude item `shouldBe` 37.6
                Barometric.longitude item `shouldBe` 139.5
                Barometric.airPressure item `shouldBe` 1000
                Barometric.geopotentialHeight item `shouldBe` 101.0
                Barometric.lagrangianTendencyOfAirPressure item `shouldBe` 0.0
                Barometric.eastwardWind item `shouldBe` 2.51
                Barometric.northwardWind item `shouldBe` -1.17
                Barometric.airTemperature item `shouldBe` -0.8
                Barometric.relativeHumidity item `shouldBe` 85
