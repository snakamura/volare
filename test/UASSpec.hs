module UASSpec (spec) where

import Control.Monad.Trans.State.Strict (evalStateT)
import Data.Maybe
    ( fromJust
    , isJust
    )
import qualified Network.HTTP.Client as Http
import qualified Network.HTTP.Client.TLS as Http
import qualified Pipes.ByteString as PB
import System.IO
    ( IOMode(ReadMode)
    , withFile
    )
import Test.Hspec
    ( Spec
    , describe
    , it
    , runIO
    , shouldBe
    , shouldSatisfy
    )

import qualified Service.UAS as UAS

import SpecUtils


spec :: Spec
spec = do
    manager <- runIO $ Http.newManager Http.tlsManagerSettings

    describe "download" $ do
        it "downloads raw data" $ do
            let station = fromJust $ UAS.station 47646
            hash <- UAS.download station 2014 04 26 0 manager sha1
            hash `shouldBe` "0b423e77d92acaef2a3245c12041721f6c0681ce"

    describe "parse" $ do
        it "succeeds in parsing a file" $ do
            withFile "test/uas_2014042600.txt" ReadMode $ \handle -> do
                observation <- evalStateT UAS.parser $ PB.fromHandle handle
                observation `shouldSatisfy` isJust
                let o = fromJust observation
                UAS.day o `shouldBe` 26
                UAS.hour o `shouldBe` 0
                UAS.stationId o `shouldBe` 47646
                let items = UAS.items o
                    surface = head items
                UAS.plane surface `shouldBe` UAS.Surface
                UAS.pressure surface `shouldBe` UAS.Pressure 1024
                UAS.temperature (UAS.entry surface) `shouldApproximatelyBe` Just 17.4
                UAS.dewPoint (UAS.entry surface) `shouldApproximatelyBe` Just 10.4
                UAS.windDirection (UAS.entry surface) `shouldBe` Just 240
                UAS.windSpeed (UAS.entry surface) `shouldBe` Just 2
                let barometric850 = items !! 7
                UAS.plane barometric850 `shouldBe` UAS.Barometric (Just 1580)
                UAS.pressure barometric850 `shouldBe` UAS.Pressure 850
                UAS.temperature (UAS.entry barometric850) `shouldApproximatelyBe` Just 5.8
                UAS.dewPoint (UAS.entry barometric850) `shouldApproximatelyBe` Just (-3.2)
                UAS.windDirection (UAS.entry barometric850) `shouldBe` Just 40
                UAS.windSpeed (UAS.entry barometric850) `shouldBe` Just 2
                let barometric638 = items !! 10
                UAS.plane barometric638 `shouldBe` UAS.Barometric Nothing
                UAS.pressure barometric638 `shouldBe` UAS.Pressure 638
                UAS.temperature (UAS.entry barometric638) `shouldApproximatelyBe` Just (-10.7)
                UAS.dewPoint (UAS.entry barometric638) `shouldApproximatelyBe` Just (-23.7)
                UAS.windDirection (UAS.entry barometric638) `shouldBe` Nothing
                UAS.windSpeed (UAS.entry barometric638) `shouldBe` Nothing
