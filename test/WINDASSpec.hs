module WINDASSpec (spec) where

import Control.Monad.Trans.State.Strict (evalStateT)
import Crypto.Hash.SHA1 (hashlazy)
import qualified Data.ByteString.Lazy as BL
import Data.Functor ((<$>))
import qualified Pipes.ByteString as PB
import System.IO
    ( IOMode(ReadMode)
    , hClose
    , withFile
    )
import System.IO.Temp (withSystemTempFile)
import Test.Hspec
import Text.Bytedump (dumpRawBS)

import qualified Service.WINDAS as WINDAS

import SpecUtils


spec :: Spec
spec = do
    describe "download" $ do
        it "downloads data" $ do
            withSystemTempFile "windas.tar.gz" $ \path handle -> do
                WINDAS.download 2014 9 14 3 $ PB.toHandle handle
                hClose handle
                hash <- hashlazy <$> BL.readFile path
                dumpRawBS hash @== "ef2a8deb36b978fcf3e6973888c94ccbb627fb73"

    describe "parser" $ do
        it "parses a single file" $ do
            let path = "test/IUPC43_RJTD_140300_201409140316313_001.send"
            stations <- withFile path ReadMode $ evalStateT WINDAS.parser . PB.fromHandle
            length stations @== 3
            let station = fst $ stations !! 1
            WINDAS.id station @== 47629
            let observations = snd $ stations !! 1
            length observations @== 6
            let observation = observations !! 2
            WINDAS.year observation @== 2014
            WINDAS.month observation @== 9
            WINDAS.day observation @== 14
            WINDAS.hour observation @== 2
            WINDAS.minute observation @== 30
            let items = WINDAS.items observation
            length items @== 16
            let item = items !! 9
            WINDAS.altitude item @== 5240
            WINDAS.eastwardWind item @== 8.3
            WINDAS.northwardWind item @== -10.3
