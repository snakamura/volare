module GeoWptSpec (spec) where

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
import qualified Pipes as P
import qualified Pipes.Attoparsec as P
import qualified Pipes.ByteString as PB
import System.IO
    ( IOMode(ReadMode)
    , withFile
    )
import Test.Hspec
    ( Spec
    , context
    , describe
    , it
    , shouldBe
    , shouldSatisfy
    )

import qualified Codec.GeoWpt as GeoWpt
import Codec.Utils.Attoparsec (line)


spec :: Spec
spec = do
    let withTestFile action = withFile "test/test.wpt" ReadMode $ action . PB.fromHandle
    let load = withTestFile $ \producer -> do
                   wpt <- evalStateT GeoWpt.parser producer
                   wpt `shouldSatisfy` isJust
                   return $ fromJust wpt

    describe "parse" $ do
        it "succeeds in pasing a wpt" $ do
            void load

        it "fails with extra inputs" $ do
            withTestFile $ \producer -> do
                wpt <- evalStateT GeoWpt.parser $ producer >> P.yield "\n"
                wpt `shouldSatisfy` isNothing

        it "fails without the first line" $ do
            withTestFile $ \producer -> do
                rest <- execStateT (P.parse line) producer
                wpt <- evalStateT GeoWpt.parser rest
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
