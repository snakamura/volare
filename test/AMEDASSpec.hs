module AMEDASSpec (spec) where

import Pipes ((>->))
import qualified Pipes
import qualified Pipes.Prelude as Pipes
import System.IO (IOMode(ReadMode),
                  hClose,
                  withFile)
import System.IO.Temp (withSystemTempFile)
import Test.Hspec

import qualified Service.AMEDAS as AMEDAS

import SpecUtils


spec :: Spec
spec = do
  describe "stations" $ do
    it "returns stations in a specified range" $ do
      length (AMEDAS.stations (37, 140) (36, 141)) `shouldBe` 16

  let download = let station = AMEDAS.Station {
                                   AMEDAS.prec = 36,
                                   AMEDAS.block = 312,
                                   AMEDAS.latitude = 36.93833,
                                   AMEDAS.longitude = 140.40834,
                                   AMEDAS.name = "Higashishirakawa"
                               }
                 in AMEDAS.download station 2014 4 26

  describe "download" $ do
    it "downloads all items" $ do
      items <- download
      length items `shouldBe` 144

    it "downloads a correct item" $ do
      items <- download
      let item = items !! 70
      AMEDAS.time item `shouldBe` 710
      AMEDAS.precipitation item `shouldBe` Just 0
      AMEDAS.temperature item `shouldBe` Just 22.8
      AMEDAS.windSpeed item `shouldBe` Just 3.1
      AMEDAS.windDirection item `shouldBe` Just AMEDAS.S
      AMEDAS.sunshine item `shouldBe` Just 10

  describe "save" $ do
    it "saves all items" $ do
      items <- download
      withSystemTempFile "amedasspec.csv" $ \path handle -> do
        Pipes.runEffect $ mapM_ Pipes.yield items >-> AMEDAS.save handle
        hClose handle
        expected <- readFile "test/amedas_36_0312_20140426.csv"
        actual <- readFile path
        actual @== expected

  describe "load" $ do
    it "loads all items" $ do
      downloadedItems <- download
      loadedItems <- withFile "test/amedas_36_0312_20140426.csv" ReadMode $ Pipes.toListM . AMEDAS.load
      loadedItems @== downloadedItems
