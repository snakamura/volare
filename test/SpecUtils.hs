module SpecUtils
    ( runDB
    , (@==)
    , (@??)
    , sha1
    ) where

import Control.Applicative ((<*))
import qualified Control.Foldl as F
import Control.Monad.IO.Class
    ( MonadIO
    , liftIO
    )
import Control.Monad.Logger
    ( NoLoggingT
    , runNoLoggingT
    )
import Control.Monad.Trans.Resource
    ( ResourceT
    , runResourceT
    )
import Crypto.Hash
    ( Digest
    , HashAlgorithm
    , SHA1
    , digestToHexByteString
    , hashFinalize
    , hashInit
    , hashUpdate
    )
import qualified Data.ByteString as B
import Database.Persist.Class
    ( PersistConfigBackend
    , applyEnv
    , createPoolConfig
    , loadConfig
    , runPool
    )
import Database.Persist.Sql
    ( runMigration
    , transactionUndo
    )
import Pipes (Producer)
import qualified Pipes.Prelude as P
import Test.HUnit
    ( (@?=)
    , assertBool
    )
import Yesod.Default.Config
    ( DefaultEnv(Testing)
    , withYamlEnvironment
    )

import qualified Volare.Model as M
import Volare.Settings (PersistConfig)


runDB :: PersistConfigBackend PersistConfig (NoLoggingT (ResourceT IO)) a ->
         IO a
runDB action = do
    persistConfig :: PersistConfig <- withYamlEnvironment "config/persist.yml" Testing loadConfig >>= applyEnv
    pool <- createPoolConfig persistConfig
    runResourceT $ runNoLoggingT $ do
        let actions = runMigration M.migrateAll >> action <* transactionUndo
        runPool persistConfig actions pool


(@==) :: (MonadIO m, Eq a, Show a) =>
         a ->
         a ->
         m ()
actual @== expected = liftIO $ actual @?= expected
infix 1 @==


(@??) :: (MonadIO m) =>
         a ->
         (a -> Bool) ->
         m ()
value @?? predicate = liftIO $ assertBool "" $ predicate value
infix 1 @??


sha1 :: Monad m =>
        Producer B.ByteString m () ->
        m B.ByteString
sha1 producer = do
    digest <- F.purely P.fold hash producer
    return $ digestToHexByteString (digest :: Digest SHA1)


hash :: HashAlgorithm a =>
        F.Fold B.ByteString (Digest a)
hash = F.Fold hashUpdate hashInit hashFinalize
