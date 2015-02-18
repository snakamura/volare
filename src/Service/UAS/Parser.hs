module Service.UAS.Parser
    ( observation
    ) where

import Codec.Utils.Attoparsec (digits)
import Control.Applicative
    ( (<*>)
    , (<*)
    , (*>)
    , (<|>)
    , many
    , pure
    )
import Control.Monad (void)
import Data.Attoparsec.ByteString
    ( Parser
    , string
    )
import Data.Attoparsec.ByteString.Char8
    ( anyChar
    , char
    , digit
    , space
    )
import Data.Attoparsec.Combinator (count)
import Data.Functor ((<$>))
import Data.Monoid
    ( First(First)
    , (<>)
    , getFirst
    )

import Service.UAS.Types
    ( Entry(Entry)
    , Item(Item)
    , Observation(Observation)
    , Plane(Surface, Barometric)
    , Pressure(Pressure)
    )
import qualified Service.UAS.Types as Types


-- $setup
-- >>> import Data.Attoparsec.ByteString (parseOnly)


observation :: Parser Observation
observation = merge <$> ttaa <*> ttbb <* many anyChar


ttaa :: Parser Observation
ttaa = uncurry Observation <$> (string " TTAA" *> space *> time <* space)
                           <*> (digits 5 <* space)
                           <*> (many ttaaItem <* many ignorable)


ttbb :: Parser Observation
ttbb = uncurry Observation <$> (string " TTBB" *> space *> time <* space)
                           <*> (digits 5 <* space)
                           <*> (many ttbbItem <* many ignorable)


time :: Parser (Int, Int)
time = (,) <$> ((subtract 50) <$> digits 2) <*> digits 2 <* digit


ttaaItem :: Parser Item
ttaaItem = uncurry Item <$> (plane <* space) <*> (makeEntry <$> (tttdd <* space) <*> (wwwss <* space))
  where
    makeEntry (t, d) (wd, ws) = Entry t d wd ws


ttbbItem :: Parser Item
ttbbItem = uncurry Item <$> (cclll <* space) <*> (uncurry Entry  <$> (tttdd <* space) <*> pure Nothing <*> pure Nothing)


-- |
-- >>> parseOnly plane "99982"
-- Right (Surface,Pressure 982)
-- >>> parseOnly plane "99020"
-- Right (Surface,Pressure 1020)
-- >>> parseOnly plane "00052"
-- Right (Barometric (Just 52),Pressure 1000)
-- >>> parseOnly plane "92912"
-- Right (Barometric (Just 912),Pressure 925)
-- >>> parseOnly plane "85585"
-- Right (Barometric (Just 1585),Pressure 850)
-- >>> parseOnly plane "70138"
-- Right (Barometric (Just 3138),Pressure 700)
-- >>> parseOnly plane "70934"
-- Right (Barometric (Just 2934),Pressure 700)
-- >>> parseOnly plane "50572"
-- Right (Barometric (Just 5720),Pressure 500)
-- >>> parseOnly plane "40736"
-- Right (Barometric (Just 7360),Pressure 400)
-- >>> parseOnly plane "30935"
-- Right (Barometric (Just 9350),Pressure 300)
-- >>> parseOnly plane "25054"
-- Right (Barometric (Just 10540),Pressure 250)
-- >>> parseOnly plane "25986"
-- Right (Barometric (Just 9860),Pressure 250)
-- >>> parseOnly plane "20193"
-- Right (Barometric (Just 11930),Pressure 200)
-- >>> parseOnly plane "15371"
-- Right (Barometric (Just 13710),Pressure 150)
-- >>> parseOnly plane "10624"
-- Right (Barometric (Just 16240),Pressure 100)
plane :: Parser (Plane, Pressure)
plane = s <|> b "00" <|> b "92" <|> b "85" <|> b "70" <|> b "50" <|> b "40" <|> b "30" <|> b "25" <|> b "20" <|> b "15" <|> b "10"
  where
    s = do
        void $ string "99"
        pressure <- digits 3
        return $ (Surface, Pressure $ pressure + if pressure > 500 then 0 else 1000)
    b i = do
        void $ string i
        height <- digits 3
        let (p, h) = case i of
                         "00" -> (1000, height)
                         "92" -> (925, height)
                         "85" -> (850, height + 1000)
                         "70" -> (700, height + if height > 500 then 2000 else 3000)
                         "50" -> (500, height * 10)
                         "40" -> (400, height * 10)
                         "30" -> (300, height * 10)
                         "25" -> (250, height * 10 + if height > 500 then 0 else 10000)
                         "20" -> (200, height * 10 + 10000)
                         "15" -> (150, height * 10 + 10000)
                         "10" -> (100, height * 10 + 10000)
                         _ -> error "Never happens."
        return (Barometric (Just h), Pressure p)


-- |
-- >>> parseOnly cclll "00024"
-- Right (Surface,Pressure 1024)
-- >>> parseOnly cclll "00983"
-- Right (Surface,Pressure 983)
-- >>> parseOnly cclll "11012"
-- Right (Barometric Nothing,Pressure 1012)
-- >>> parseOnly cclll "55823"
-- Right (Barometric Nothing,Pressure 823)
cclll :: Parser (Plane, Pressure)
cclll = s <|> b
  where
    s = do
        void $ string "00"
        pressure <- digits 3
        return $ (Surface, Pressure $ pressure + if pressure > 500 then 0 else 1000)
    b = do
        void $ string "11" <|> string "22" <|> string "33" <|> string "44" <|> string "55" <|> string "66" <|> string "77" <|> string "88" <|> string "99"
        pressure <- digits 3
        return (Barometric Nothing, Pressure $ pressure + if pressure < 100 then 1000 else 0)


-- |
-- >>> parseOnly tttdd "04108"
-- Right (Just (-4.1),Just (-4.9))
-- >>> parseOnly tttdd "04858"
-- Right (Just 4.8,Just (-3.1999998))
-- >>> parseOnly tttdd "/////"
-- Right (Nothing,Nothing)
tttdd :: Parser (Maybe Float, Maybe Float)
tttdd = do
    t <- ttt <|> (string "///" >> return Nothing)
    d <- dd <|> (string "//" >> return Nothing)
    return (t, (-) <$> t <*> d)
  where
    ttt = do
        t <- digits 3
        return $ Just $ (if even t then id else negate) $ fromIntegral t / 10
    dd = do
        d <- digits 2
        return $ Just $ if d > 50 then fromIntegral (d - 50) else fromIntegral d / 10


-- |
-- >>> parseOnly wwwss "33015"
-- Right (Just 330,Just 15)
-- >>> parseOnly wwwss "12622"
-- Right (Just 125,Just 122)
-- >>> parseOnly wwwss "/////"
-- Right (Nothing,Nothing)
wwwss :: Parser (Maybe Int, Maybe Int)
wwwss = do
    w <- www <|> (string "///" >> return Nothing)
    s <- ss <|> (string "//" >> return Nothing)
    return (fst <$> w, (+) <$> s <*> (snd <$> w))
  where
    www = do
        d <- digits 3
        return $ Just $ if d `mod` 5 == 0 then (d, 0) else (d - 1, 100)
    ss = Just <$> digits 2


ignorable :: Parser ()
ignorable = void $ count 5 (digit <|> char '/') >> space


merge :: Observation ->
         Observation ->
         Observation
merge o1 o2 | o1 { Types.items = [] } /= o2 { Types.items = [] } = o1
merge o1 o2 = o1 { Types.items = mergeItems (Types.items o1) (Types.items o2) }
  where
    mergeItems (item1:items1) (item2:items2) =
        case compareItem item1 item2 of
            LT -> item1:mergeItems items1 (item2:items2)
            GT -> item2:mergeItems (item1:items1) items2
            EQ -> mergeItem item1 item2:mergeItems items1 items2
    mergeItems items1 [] = items1
    mergeItems [] items2 = items2
    mergeItem (Item Surface p entry1) (Item Surface _ entry2) = Item Surface p $ mergeEntry entry1 entry2
    mergeItem (Item (Barometric h1) p entry1) (Item (Barometric h2) _ entry2) = Item (Barometric $ getFirst $ First h1 <> First h2) p $ mergeEntry entry1 entry2
    mergeItem _ _ = error "Never happens."
    mergeEntry entry1 entry2 = Entry (getFirst (First (Types.temperature entry1) <> First (Types.temperature entry2)))
                                     (getFirst (First (Types.dewPoint entry1) <> First (Types.dewPoint entry2)))
                                     (getFirst (First (Types.windDirection entry1) <> First (Types.windDirection entry2)))
                                     (getFirst (First (Types.windSpeed entry1) <> First (Types.windSpeed entry2)))


compareItem :: Item ->
               Item ->
               Ordering
compareItem (Item Surface _ _)         (Item Surface _ _)         = EQ
compareItem (Item Surface _ _)         _                          = LT
compareItem _                          (Item Surface _ _)         = GT
compareItem (Item (Barometric _) p1 _) (Item (Barometric _) p2 _) = compare p2 p1
