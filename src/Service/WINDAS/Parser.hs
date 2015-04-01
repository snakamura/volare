module Service.WINDAS.Parser
    ( parser
    ) where

import Control.Exception (Exception)
import Control.Monad
    ( forever
    , void
    , when
    )
import Control.Monad.Catch
    ( MonadThrow
    , throwM
    )
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict
    ( evalStateT
    , get
    )
import Data.Bits
    ( Bits
    , FiniteBits
    , finiteBitSize
    , setBit
    , shiftL
    , testBit
    , zeroBits
    )
import qualified Data.ByteString as B
import Data.List (foldl')
import Data.Maybe (catMaybes)
import qualified Data.Text as T
import Data.Word (Word8)
import Data.Typeable (Typeable)
import Lens.Family2.State.Strict (zoom)
import Pipes ((>->))
import qualified Pipes as P
import qualified Pipes.Parse as P

import Service.WINDAS.Stations (station)
import Service.WINDAS.Types
    ( Station
    , Observation(Observation)
    , Item(Item)
    )


-- $setup
-- >>> import Data.Word (Word32)
-- >>> import Test.QuickCheck
-- >>>
-- >>> instance Arbitrary Bit where arbitrary = toEnum . (`mod` 2) <$> arbitrary
-- >>> newtype Bits = Bits { getBits :: [Bit] } deriving Show
-- >>> instance Arbitrary Bits where arbitrary = Bits <$> resize (finiteBitSize (undefined :: Word32)) arbitrary
-- >>>
-- >>> let fillBits n bits = replicate (n - length bits) Zero ++ bits


data Bit = Zero
         | One
    deriving (Show, Eq, Enum)


data ParseException = ParseException T.Text deriving (Show, Eq, Typeable)

instance Exception ParseException


parser :: (Functor m, Monad m, MonadThrow m) =>
          P.Parser B.ByteString m [(Station, [Observation])]
parser = do
    producer <- get
    lift $ evalStateT bitParser $ producer >-> bytesToBits


bitParser :: (Functor m, Monad m, MonadThrow m) =>
             P.Parser Bit m [(Station, [Observation])]
bitParser = do
    skip $ 18 * 8
    bufr <- drawText 4
    if bufr == "BUFR" then
        bufrParser
    else do
        bufr2 <- drawText 4
        if bufr2 == "BUFR" then
            bufrParser
        else
            throwM $ ParseException "Invalid format"
  where
    bufrParser = do
        skip $ 64 - 4 * 8
        len1 <- drawInt $ 3 * 8
        skip $ (len1 - 3) * 8
        len3 <- drawInt $ 3 * 8
        skip 8
        stationCount <- drawInt 16
        skip $ (len3 - (3 + 1 + 2)) * 8
        len4 <- drawInt $ 3 * 8
        stations <- zoom (P.splitAt ((len4 - 3) * 8)) $
            skip 8 *> sequence (replicate stationCount stationParser) <* void P.drawAll
        end <- sequence $ replicate 4 $ drawChar 8
        when (end /= "7777") $
            throwM $ ParseException "Invalid BUFR"
        return stations
    stationParser = do
        block <- drawInt 7
        location <- drawInt 10
        _latitude :: Float <- convert 100 (-9000) <$> drawInt 15
        _longitude :: Float <- convert 100 (-18000) <$> drawInt 16
        _altitude <- (subtract 400) <$> drawInt 15
        _equipment <- drawInt 4
        let s = station $ block * 1000 + location
        case s of
            Just s' -> do
                x <- drawInt 8
                observations <- sequence $ replicate x observationParser
                return (s', observations)
            Nothing -> throwM $ ParseException "Unknown station"
    observationParser = do
        year <- drawInt 12
        month <- drawInt 4
        day <- drawInt 6
        hour <- drawInt 5
        minute <- drawInt 6
        skip $ 5 + 12
        y <- drawInt 8
        items <- sequence $ replicate y itemParser
        return $ Observation year month day hour minute (catMaybes items)
    itemParser = do
        altitude <- drawInt 15
        quality <- drawInt 8
        eastwardWind <- convert 10 (-4096) <$> drawInt 13
        northwardWind <- convert 10 (-4096) <$> drawInt 13
        verticalWind <- convert 100 (-4096) <$> drawInt 13
        skip 8
        return $ if quality == 0x80 then
                     Just $ Item altitude eastwardWind northwardWind verticalWind
                 else
                     Nothing
    convert scale reference n = fromIntegral (n + reference) / scale


skip :: (Functor m, Monad m) =>
        Int ->
        P.Parser Bit m ()
skip n = void $ zoom (P.splitAt n) P.drawAll


drawText :: (Functor m, Monad m) =>
            Int ->
            P.Parser Bit m T.Text
drawText n = T.pack <$> sequence (replicate n $ drawChar 8)


drawChar :: (Functor m, Monad m) =>
            Int ->
            P.Parser Bit m Char
drawChar n = charFromBits <$> zoom (P.splitAt n) P.drawAll


drawInt :: (Functor m, Monad m) =>
           Int ->
           P.Parser Bit m Int
drawInt n = intFromBits <$> zoom (P.splitAt n) P.drawAll


bytesToBits :: Monad m =>
               P.Pipe B.ByteString Bit m r
bytesToBits = forever $ P.await >>= P.each . concatMap toBits . B.unpack


-- |
-- >>> toBits (10 :: Word8)
-- [Zero,Zero,Zero,Zero,One,Zero,One,Zero]
--
-- prop> \n -> fromBits (toBits n) == (n :: Word32)
toBits :: FiniteBits b =>
          b ->
          [Bit]
toBits b = let s = finiteBitSize b
           in map (toEnum . fromEnum . testBit b) [s - 1, s - 2 .. 0]


-- |
-- >>> fromBits [Zero,Zero,Zero,Zero,One,Zero,One,Zero] :: Word8
-- 10
--
-- prop> \bits -> toBits (fromBits (getBits bits) :: Word32) == fillBits (finiteBitSize (undefined :: Word32)) (getBits bits)
fromBits :: Bits b =>
            [Bit] ->
            b
fromBits bits = foldl' f zeroBits bits
  where
    f b Zero = shiftL b 1
    f b One = setBit (shiftL b 1) 0


-- |
-- >>> charFromBits [One,Zero,Zero,Zero,Zero,Zero,One]
-- 'A'
charFromBits :: [Bit] ->
                Char
charFromBits = toEnum . fromEnum . word8FromBits


-- |
-- >>> intFromBits [One,Zero,Zero,Zero,Zero,Zero,One]
-- 65
intFromBits :: [Bit] ->
               Int
intFromBits = fromBits


-- |
-- >>> word8FromBits [One,Zero,Zero,Zero,Zero,Zero,One]
-- 65
word8FromBits :: [Bit] ->
                 Word8
word8FromBits bits | length bits <= 8 = fromBits bits
                   | otherwise = error "Invalid bits"
