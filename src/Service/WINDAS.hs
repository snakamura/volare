module Service.WINDAS
    ( Types.Station(..)
    , Types.Observation(..)
    , Types.Item(..)
    , parser
    , download
    , downloadArchive
    , Stations.station
    , Stations.stations
    ) where

import qualified Codec.Archive.Tar as Tar
import Codec.Compression.GZip (decompress)
import Control.Exception (throwIO)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class
    ( MonadIO
    , liftIO
    )
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict (evalStateT)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Functor ((<$>))
import Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import Formatting ((%))
import qualified Formatting as F
import qualified Network.HTTP.Client as Http
import Pipes
    ( Consumer
    , Producer
    , (>->)
    , each
    , runEffect
    )
import qualified Pipes.ByteString as PB
import Pipes.HTTP (withHTTP)
import qualified Pipes.Prelude as P
import System.IO.Error
    ( doesNotExistErrorType
    , mkIOError
    )
import Text.HTML.TagSoup
    ( fromAttrib
    , isTagOpenName
    , parseTags)

import Service.WINDAS.Parser (parser)
import qualified Service.WINDAS.Types as Types
import qualified Service.WINDAS.Stations as Stations


download :: Types.Station ->
            Int ->
            Int ->
            Int ->
            Int ->
            Consumer Types.Observation IO () ->
            IO ()
download station year month day hour consumer =
    downloadArchive year month day hour $ \producer -> do
        entries <- Tar.read . decompress . BL.fromChunks <$> P.toListM producer
        case Tar.foldEntries checkEntry Nothing (const Nothing) entries of
            Just contents -> do
                stations <- evalStateT parser $ PB.fromLazy contents
                case lookup station stations of
                    Just observations -> runEffect $ each observations >-> consumer
                    Nothing -> throwIO $ mkIOError doesNotExistErrorType "Station not found" Nothing Nothing
            Nothing -> throwIO $ mkIOError doesNotExistErrorType "Entry not found" Nothing Nothing
  where
    checkEntry _ (Just e) = Just e
    checkEntry entry Nothing | name `TL.isPrefixOf` TL.pack (Tar.entryPath entry)
                             , Tar.NormalFile b _ <- Tar.entryContent entry = Just b
                             | otherwise = Nothing
    name = F.format ("IUPC" % F.left 2 '0' % "_RJTD_") (Types.message station)


downloadArchive :: Int ->
                   Int ->
                   Int ->
                   Int ->
                   (Producer B.ByteString IO () -> IO r) ->
                   IO r
downloadArchive year month day hour process = do
    file <- P.find isHour $ listFiles year month day
    case file of
        Just name -> do
            let url = baseURL year month day <> TL.fromStrict name
            req <- Http.parseUrl $ TL.unpack url
            liftIO $ Http.withManager Http.defaultManagerSettings $ \manager -> do
                withHTTP req manager $ \res -> do
                    process $ Http.responseBody res
        Nothing -> throwIO $ mkIOError doesNotExistErrorType "File not found" Nothing Nothing
  where
    isHour file = let s = F.sformat ("IUPC00_COMP_" % F.left 4 '0' % F.left 2 '0' % F.left 2 '0' % F.left 2 '0') year month day hour
                  in s `T.isInfixOf` file


listFiles :: (MonadIO m, MonadThrow m) =>
             Int ->
             Int ->
             Int ->
             Producer T.Text m ()
listFiles year month day = do
    req <- lift $ Http.parseUrl $ TL.unpack $ baseURL year month day
    res <- liftIO $ Http.withManager Http.defaultManagerSettings $ Http.httpLbs req
    each $ parseDirectory $ Http.responseBody res


parseDirectory :: BL.ByteString ->
                  [T.Text]
parseDirectory = map (T.decodeUtf8 . BL.toStrict)
                     . filter (".tar.gz" `BL.isSuffixOf`)
                     . map (fromAttrib "href")
                     . filter (isTagOpenName "a")
                     . parseTags


baseURL :: Int ->
           Int ->
           Int ->
           TL.Text
baseURL year month day = let url = "http://database.rish.kyoto-u.ac.jp/arch/jmadata/data/jma-radar/wprof/original/"
                         in F.format (url % F.left 4 '0' % "/" % F.left 2 '0' % "/" % F.left 2 '0' % "/") year month day
