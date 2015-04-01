module AMEDASSpec (spec) where

import Data.Maybe (fromJust)
import Pipes ((>->))
import qualified Pipes as P
import qualified Pipes.ByteString as PB
import qualified Pipes.Prelude as P
import System.IO
    ( IOMode(ReadMode)
    , hClose
    , withFile
    )
import System.IO.Temp (withSystemTempFile)
import Test.Hspec
    ( Spec
    , describe
    , it
    , shouldBe
    )

import qualified Service.AMEDAS as AMEDAS


spec :: Spec
spec = do
    describe "stations" $ do
        it "returns stations in a specified range" $ do
            length (AMEDAS.stations (37, 140) (36, 141)) `shouldBe` 16

    let downloadSimple = let station = fromJust $ AMEDAS.station "Higashishirakawa"
                         in AMEDAS.download station 2014 4 26

    let downloadComplex = let station = fromJust $ AMEDAS.station "Tsukuba"
                          in AMEDAS.download station 2014 4 26

    describe "download" $ do
        it "downloads all simple items" $ do
            count <- P.length downloadSimple
            count `shouldBe` 144

        it "downloads all complex items" $ do
            count <- P.length downloadComplex
            count `shouldBe` 144

        it "downloads a correct simple item" $ do
            item <- fromJust <$> P.index 70 downloadSimple
            AMEDAS.time item `shouldBe` 710
            AMEDAS.precipitation item `shouldBe` Just 0
            AMEDAS.temperature item `shouldBe` Just 22.8
            AMEDAS.windSpeed item `shouldBe` Just 3.1
            AMEDAS.windDirection item `shouldBe` Just AMEDAS.S
            AMEDAS.sunshine item `shouldBe` Just 10

        it "downloads a correct complex item" $ do
            item <- fromJust <$> P.index 10 downloadComplex
            AMEDAS.time item `shouldBe` 110
            AMEDAS.precipitation item `shouldBe` Nothing
            AMEDAS.temperature item `shouldBe` Just 9.8
            AMEDAS.windSpeed item `shouldBe` Just 1.4
            AMEDAS.windDirection item `shouldBe` Just AMEDAS.ENE
            AMEDAS.sunshine item `shouldBe` Nothing

    describe "save" $ do
        it "saves all items" $ do
            withSystemTempFile "amedasspec.csv" $ \path handle -> do
                P.runEffect $ downloadSimple >-> AMEDAS.save (PB.toHandle handle)
                hClose handle
                expected <- readFile "test/amedas_36_0312_20140426.csv"
                actual <- readFile path
                actual `shouldBe` expected

    describe "load" $ do
        it "loads all simple items" $ do
            l <- withFile "test/amedas_36_0312_20140426.csv" ReadMode $ \handle -> do
                P.toListM $ P.zipWith (==) downloadSimple (AMEDAS.load $ PB.fromHandle handle)
            length l `shouldBe` 144
            and l `shouldBe` True

        it "loads all complex items" $ do
            l <- withFile "test/amedas_40_47646_20140426.csv" ReadMode $ \handle -> do
                P.toListM $ P.zipWith (==) downloadComplex (AMEDAS.load $ PB.fromHandle handle)
            length l `shouldBe` 144
            and l `shouldBe` True
