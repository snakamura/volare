module Service.WINDAS.Parser
    ( parser
    ) where

import Control.Applicative
    ( (*>)
    , (<*)
    )
import Control.Monad
    ( forever
    , void
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
import Data.Functor ((<$>))
import Data.List (foldl')
import Data.Maybe (catMaybes)
import Data.Word (Word8)
import Lens.Family2.State.Strict (zoom)
import Pipes ((>->))
import qualified Pipes as P
import qualified Pipes.Parse as P

import Service.WINDAS.Types
    ( Location(Location)
    , Observation(Observation)
    , Item(Item)
    )


data Bit = Zero
         | One
    deriving (Show, Eq, Enum)


parser :: (Functor m, Monad m) =>
          P.Parser B.ByteString m (Either String [(Location, [Observation])])
parser = do
    producer <- get
    lift $ evalStateT bitParser $ producer >-> bytesToBits


bitParser :: (Functor m, Monad m) =>
             P.Parser Bit m (Either String [(Location, [Observation])])
bitParser = do
    skip $ 18 * 8
    bufr <- drawString 4
    if bufr == "BUFR" then
        bufrParser
    else do
        bufr2 <- drawString 4
        if bufr2 == "BUFR" then
            bufrParser
        else
            return $ Left "Invalid format"
  where
    bufrParser = do
        skip $ 64 - 4 * 8
        len1 <- drawInt $ 3 * 8
        skip $ (len1 - 3) * 8
        len3 <- drawInt $ 3 * 8
        skip 8
        locationCount <- drawInt 16
        skip $ (len3 - (3 + 1 + 2)) * 8
        len4 <- drawInt $ 3 * 8
        locations <- zoom (P.splitAt ((len4 - 3) * 8)) $
            skip 8 *> sequence (replicate locationCount locationParser) <* void P.drawAll
        end <- sequence $ replicate 4 $ drawChar 8
        return $ if end == "7777" then
                     Right locations
                 else
                     Left "Invalid BUFR"
    locationParser = do
        block <- drawInt 7
        station <- drawInt 10
        latitude <- convert 100 (-9000) <$> drawInt 15
        longitude <- convert 100 (-18000) <$> drawInt 16
        altitude <- (subtract 400) <$> drawInt 15
        _equipment <- drawInt 4
        let location = Location (block * 1000 + station) latitude longitude altitude
        x <- drawInt 8
        observations <- sequence $ replicate x observationParser
        return (location, observations)
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


drawString :: (Functor m, Monad m) =>
              Int ->
              P.Parser Bit m String
drawString n = sequence $ replicate n $ drawChar 8


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


toBits :: FiniteBits b =>
          b ->
          [Bit]
toBits b = let s = finiteBitSize b
           in map (toEnum . fromEnum . testBit b) [s - 1, s - 2 .. 0]


fromBits :: Bits b =>
            [Bit] ->
            b
fromBits bits = foldl' f zeroBits bits
  where
    f b Zero = shiftL b 1
    f b One = setBit (shiftL b 1) 0


charFromBits :: [Bit] ->
                Char
charFromBits = toEnum . fromEnum . word8FromBits


intFromBits :: [Bit] ->
               Int
intFromBits = fromBits


word8FromBits :: [Bit] ->
                 Word8
word8FromBits bits | length bits <= 8 = fromBits bits
                   | otherwise = error "Invalid bits"
