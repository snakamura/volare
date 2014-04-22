module IGCSpec (spec) where

import Data.Attoparsec (parseOnly)
import qualified Data.ByteString as B
import Data.Time (fromGregorian)
import Test.Hspec
import Test.Hspec.Expectations.Contrib

import qualified Codec.IGC as IGC


spec :: Spec
spec =
    context "when load from a file" $ do
        let load = do file <- B.readFile "test/test.igc"
                      let r = parseOnly IGC.igc file
                      r `shouldSatisfy` isRight
                      return $ either undefined id r

        describe "date" $ do
            it "returns HFDTE" $ do
                igc <- load
                IGC.date igc `shouldBe` fromGregorian 2014 3 15

        describe "records" $ do
            it "returns all Bs" $ do
                igc <- load
                length (IGC.records igc) `shouldBe` 5175

        describe "the first record" $ do
            it "returns the first B" $ do
                igc <- load
                let record = head $ IGC.records igc
                IGC.time record `shouldBe` 7475
                let position = IGC.position record
                IGC.latitude position `shouldBe` 36.318283
                IGC.longitude position `shouldBe` 140.18837
                IGC.altitude position `shouldBe` 388
