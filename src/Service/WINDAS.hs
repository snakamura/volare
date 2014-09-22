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
import Data.List (isPrefixOf)
import Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
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
import Text.Printf (printf)

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
    checkEntry entry Nothing | name `isPrefixOf` Tar.entryPath entry
                             , Tar.NormalFile b _ <- Tar.entryContent entry = Just b
                             | otherwise = Nothing
    name = printf "IUPC%02d_RJTD_" $ Types.message station


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
            let url = baseURL year month day <> T.unpack name
            req <- Http.parseUrl url
            liftIO $ Http.withManager Http.defaultManagerSettings $ \manager -> do
                withHTTP req manager $ \res -> do
                    process $ Http.responseBody res
        Nothing -> throwIO $ mkIOError doesNotExistErrorType "File not found" Nothing Nothing
  where
    isHour file = let s = printf "IUPC00_COMP_%04d%02d%02d%02d" year month day hour
                  in T.pack s `T.isInfixOf` file


listFiles :: (MonadIO m, MonadThrow m) =>
             Int ->
             Int ->
             Int ->
             Producer T.Text m ()
listFiles year month day = do
    req <- lift $ Http.parseUrl $ baseURL year month day
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
           String
baseURL year month day = printf "http://database.rish.kyoto-u.ac.jp/arch/jmadata/data/jma-radar/wprof/original/%04d/%02d/%02d/" year month day
