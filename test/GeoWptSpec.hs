module GeoWptSpec (spec) where

import Data.Attoparsec (parseOnly)
import qualified Data.ByteString as B
import Test.Hspec
import Test.Hspec.Expectations.Contrib

import qualified Codec.GeoWpt as GeoWpt


spec :: Spec
spec =
    context "when load from a file" $ do
        let load = do file <- B.readFile "test/test.wpt"
                      let r = parseOnly GeoWpt.wpt file
                      r `shouldSatisfy` isRight
                      return $ either undefined id r
        describe "items" $ do
            it "returns all items" $ do
                wpt <- load
                length (GeoWpt.items wpt) `shouldBe` 122
        describe "the first item" $ do
            it "returns the first item" $ do
                wpt <- load
                let item = head $ GeoWpt.items wpt
                GeoWpt.name item `shouldBe` "AAT057"
                GeoWpt.latitude item `shouldBe` 36.276783
                GeoWpt.longitude item `shouldBe` 140.14542
                GeoWpt.altitude item `shouldBe` 570
                GeoWpt.description item `shouldBe` "ASI"
