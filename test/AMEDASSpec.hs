module AMEDASSpec (spec) where

import Data.List (find)
import Data.Maybe (fromJust)
import Pipes ((>->))
import qualified Pipes
import qualified Pipes.Prelude as Pipes
import System.IO
    ( IOMode(ReadMode)
    , hClose
    , withFile
    )
import System.IO.Temp (withSystemTempFile)
import Test.Hspec

import qualified Service.AMEDAS as AMEDAS

import SpecUtils


spec :: Spec
spec = do
    describe "stations" $ do
        it "returns stations in a specified range" $ do
            length (AMEDAS.stations (37, 140) (36, 141)) `shouldBe` 16

    let downloadSimple = let station = fromJust $ find ((== "Higashishirakawa") . AMEDAS.name) AMEDAS.allStations
                         in AMEDAS.download station 2014 4 26

    let downloadComplex = let station = fromJust $ find ((== "Tsukuba") . AMEDAS.name) AMEDAS.allStations
                          in AMEDAS.download station 2014 4 26

    describe "download" $ do
        it "downloads all simple items" $ do
            items <- downloadSimple
            length items `shouldBe` 144

        it "downloads all complex items" $ do
            items <- downloadComplex
            length items `shouldBe` 144

        it "downloads a correct simple item" $ do
            items <- downloadSimple
            let item = items !! 70
            AMEDAS.time item `shouldBe` 710
            AMEDAS.precipitation item `shouldBe` Just 0
            AMEDAS.temperature item `shouldBe` Just 22.8
            AMEDAS.windSpeed item `shouldBe` Just 3.1
            AMEDAS.windDirection item `shouldBe` Just AMEDAS.S
            AMEDAS.sunshine item `shouldBe` Just 10

        it "downloads a correct complex item" $ do
            items <- downloadComplex
            let item = items !! 10
            AMEDAS.time item `shouldBe` 110
            AMEDAS.precipitation item `shouldBe` Nothing
            AMEDAS.temperature item `shouldBe` Just 9.8
            AMEDAS.windSpeed item `shouldBe` Just 1.4
            AMEDAS.windDirection item `shouldBe` Just AMEDAS.ENE
            AMEDAS.sunshine item `shouldBe` Nothing

    describe "save" $ do
        it "saves all items" $ do
            items <- downloadSimple
            withSystemTempFile "amedasspec.csv" $ \path handle -> do
                Pipes.runEffect $ mapM_ Pipes.yield items >-> AMEDAS.save handle
                hClose handle
                expected <- readFile "test/amedas_36_0312_20140426.csv"
                actual <- readFile path
                actual @== expected

    describe "load" $ do
        it "loads all simple items" $ do
            downloadedItems <- downloadSimple
            loadedItems <- withFile "test/amedas_36_0312_20140426.csv" ReadMode $ Pipes.toListM . AMEDAS.load
            loadedItems @== downloadedItems

        it "loads all complex items" $ do
            downloadedItems <- downloadComplex
            loadedItems <- withFile "test/amedas_40_47646_20140426.csv" ReadMode $ Pipes.toListM . AMEDAS.load
            loadedItems @== downloadedItems
