module GeoWptSpec (spec) where

import Control.Monad (void)
import Control.Monad.Trans.State.Strict (evalStateT)
import Data.Maybe
    ( fromJust
    , isJust
    , isNothing
    )
import qualified Pipes as P
import qualified Pipes.ByteString as P
import System.IO
    ( IOMode(ReadMode)
    , withFile
    )
import Test.Hspec

import qualified Codec.GeoWpt as GeoWpt


spec :: Spec
spec = do
    let load = withFile "test/test.wpt" ReadMode $ \handle -> do
                   wpt <- evalStateT GeoWpt.parser $ P.fromHandle handle
                   wpt `shouldSatisfy` isJust
                   return $ fromJust wpt

    describe "parse" $ do
        it "succeeds in pasing a wpt" $ do
            void load

        it "fails with extra inputs" $ do
            withFile "test/test.wpt" ReadMode $ \handle -> do
                wpt <- evalStateT GeoWpt.parser $ P.fromHandle handle >> P.yield "\n"
                wpt `shouldSatisfy` isNothing

    context "when load from a file" $ do
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
