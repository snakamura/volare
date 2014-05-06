module IGCSpec (spec) where

import Control.Monad (void)
import Control.Monad.Trans.State.Strict (evalStateT)
import Data.Maybe
    ( fromJust
    , isJust
    , isNothing
    )
import Data.Time (fromGregorian)
import qualified Pipes as P
import qualified Pipes.ByteString as P
import System.IO
    ( IOMode(ReadMode)
    , withFile
    )
import Test.Hspec

import qualified Codec.IGC as IGC


spec :: Spec
spec = do
    let load = withFile "test/test.igc" ReadMode $ \handle -> do
                   igc <- evalStateT IGC.parser $ P.fromHandle handle
                   igc `shouldSatisfy` isJust
                   return $ fromJust igc

    describe "parse" $ do
        it "succeeds in pasing an igc" $ do
            void load

        it "fails with extra inputs" $ do
            withFile "test/test.igc" ReadMode $ \handle -> do
                wpt <- evalStateT IGC.parser $ P.fromHandle handle >> P.yield "\n"
                wpt `shouldSatisfy` isNothing

    context "when load from a file" $ do
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
