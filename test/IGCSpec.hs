module IGCSpec (spec) where

import Control.Monad (void)
import Control.Monad.Trans.State.Strict
    ( evalStateT
    , execStateT
    )
import Data.Maybe
    ( fromJust
    , isJust
    , isNothing
    )
import Data.Time (fromGregorian)
import qualified Pipes as P
import qualified Pipes.Attoparsec as P
import qualified Pipes.ByteString as P
import System.IO
    ( IOMode(ReadMode)
    , withFile
    )
import Test.Hspec

import qualified Codec.IGC as IGC
import Codec.Utils.Attoparsec (line)


spec :: Spec
spec = do
    let withTestFile action = withFile "test/test.igc" ReadMode $ action . P.fromHandle
    let load = withTestFile $ \producer -> do
                   igc <- evalStateT IGC.parser producer
                   igc `shouldSatisfy` isJust
                   return $ fromJust igc

    describe "parse" $ do
        it "succeeds in pasing an igc" $ do
            void load

        it "fails with extra inputs" $ do
            withTestFile $ \producer -> do
                igc <- evalStateT IGC.parser $ producer >> P.yield "\n"
                igc `shouldSatisfy` isNothing

        it "fails without the first line" $ do
            withTestFile $ \producer -> do
                rest <- execStateT (P.parse line) producer
                igc <- evalStateT IGC.parser rest
                igc `shouldSatisfy` isNothing

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
