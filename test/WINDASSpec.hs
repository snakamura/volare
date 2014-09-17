module WINDASSpec (spec) where

import Crypto.Hash.SHA1 (hashlazy)
import qualified Data.ByteString.Lazy as BL
import Data.Functor ((<$>))
import qualified Pipes.ByteString as P
import System.IO (hClose)
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
                WINDAS.download 2014 9 14 3 $ P.toHandle handle
                hClose handle
                hash <- hashlazy <$> BL.readFile path
                dumpRawBS hash @== "ef2a8deb36b978fcf3e6973888c94ccbb627fb73"
