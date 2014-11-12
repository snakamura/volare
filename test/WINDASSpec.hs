module WINDASSpec (spec) where

import Control.Monad.Trans.State.Strict (evalStateT)
import Data.Maybe (catMaybes)
import Pipes
    ( (>->)
    , await
    , runEffect
    )
import qualified Pipes.ByteString as PB
import qualified Pipes.Prelude as P
import System.IO
    ( IOMode(ReadMode)
    , withFile
    )
import Test.Hspec

import qualified Service.WINDAS as WINDAS

import SpecUtils


spec :: Spec
spec = do
    describe "downloadArchive" $ do
        it "downloads an archive" $ do
            hash <- WINDAS.downloadArchive 2014 9 14 3 sha1
            hash @== "ef2a8deb36b978fcf3e6973888c94ccbb627fb73"

    describe "parseStation" $ do
        it "parses observations for a specified station" $ do
            let path = "test/IUPC00_COMP_201409140326300_010_37112.send.tar.gz"
                Just station = WINDAS.station 47629
            withFile path ReadMode $ \handle ->
                runEffect $ WINDAS.parseStation station (PB.fromHandle handle) >-> P.drop 2 >-> do
                    observation <- await
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

    describe "parseStations" $ do
        it "parses observations for specified stations" $ do
            let path = "test/IUPC00_COMP_201409140326300_010_37112.send.tar.gz"
                stations = catMaybes [WINDAS.station 47629, WINDAS.station 47626]
            observations <- withFile path ReadMode $ P.toListM . WINDAS.parseStations stations . PB.fromHandle
            length observations @== 12
            let (station, observation) = observations !! 1
            WINDAS.id station @== 47626
            WINDAS.year observation @== 2014
            WINDAS.month observation @== 9
            WINDAS.day observation @== 14
            WINDAS.hour observation @== 2
            WINDAS.minute observation @== 20
            let items = WINDAS.items observation
            length items @== 13
            let item = items !! 12
            WINDAS.altitude item @== 4366
            WINDAS.eastwardWind item @== 4.9
            WINDAS.northwardWind item @== -11.0

    describe "parseAll" $ do
        it "parses observations for all the stations" $ do
            let path = "test/IUPC00_COMP_201409140326300_010_37112.send.tar.gz"
            observations <- withFile path ReadMode $ P.toListM . WINDAS.parseAll . PB.fromHandle
            length observations @== 198
            let (station, observation) = observations !! 20
            WINDAS.id station @== 47570
            WINDAS.year observation @== 2014
            WINDAS.month observation @== 9
            WINDAS.day observation @== 14
            WINDAS.hour observation @== 2
            WINDAS.minute observation @== 30
            let items = WINDAS.items observation
            length items @== 11
            let item = items !! 7
            WINDAS.altitude item @== 3350
            WINDAS.eastwardWind item @== 4.8
            WINDAS.northwardWind item @== -5.5

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
