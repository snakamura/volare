module IGCSpec (spec) where

import Control.Monad.Trans.State.Strict (evalStateT)
import Data.Maybe
    ( fromJust
    , isJust
    )
import Data.Time (fromGregorian)
import qualified Pipes.ByteString as P
import System.IO
    ( IOMode(ReadMode)
    , withFile
    )
import Test.Hspec

import qualified Codec.IGC as IGC


spec :: Spec
spec =
    context "when load from a file" $ do
        let load = withFile "test/test.igc" ReadMode $ \handle -> do
                       igc <- evalStateT IGC.parser (P.fromHandle handle)
                       igc `shouldSatisfy` isJust
                       return $ fromJust igc

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
