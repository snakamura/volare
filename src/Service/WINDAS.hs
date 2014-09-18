module Service.WINDAS
    ( download
    ) where

import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class
    ( MonadIO
    , liftIO
    )
import Control.Monad.Trans.Class (lift)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Foldable (forM_)
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
import Pipes.HTTP (withHTTP)
import qualified Pipes.Prelude as P
import Text.HTML.TagSoup
    ( fromAttrib
    , isTagOpenName
    , parseTags)
import Text.Printf (printf)


download :: Int ->
            Int ->
            Int ->
            Int ->
            Consumer B.ByteString IO () ->
            IO ()
download year month day hour consumer = do
    file <- P.find isHour $ listFiles year month day
    forM_ file $ \name -> do
        let url = baseURL year month day <> T.unpack name
        req <- Http.parseUrl url
        liftIO $ Http.withManager Http.defaultManagerSettings $ \manager -> do
            withHTTP req manager $ \res -> do
                runEffect $ Http.responseBody res >-> consumer
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
