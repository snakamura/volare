module SpecUtils
    ( runDB
    , shouldBe'
    , shouldSatisfy'
    , shouldApproximatelyBe
    , shouldApproximatelyBe'
    , sha1
    ) where

import qualified Control.Foldl as F
import Control.Monad (unless)
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
import Data.AEq
    ( AEq
    , (~==)
    )
import qualified Data.ByteString as B
import Database.Persist.Class
    ( PersistConfigBackend
    , createPoolConfig
    , runPool
    )
import Database.Persist.Sql
    ( runMigration
    , transactionUndo
    )
import Pipes (Producer)
import qualified Pipes.Prelude as P
import Test.Hspec
    ( Expectation
    , shouldBe
    , shouldSatisfy
    )
import Test.HUnit (assertFailure)
import Yesod.Default.Config2
    ( configSettingsYml
    , loadYamlSettings
    , useEnv
    )

import qualified Volare.Model as M
import Volare.Settings (PersistConfig)
import qualified Volare.Settings as Settings


runDB :: PersistConfigBackend PersistConfig (NoLoggingT (ResourceT IO)) a ->
         IO a
runDB action = do
    settings <- loadYamlSettings ["config/settings_test.yml", configSettingsYml] [] useEnv
    let persistConfig = Settings.persistConfig settings
    pool <- createPoolConfig persistConfig
    runResourceT $ runNoLoggingT $ do
        let actions = runMigration M.migrateAll >> action <* transactionUndo
        runPool persistConfig actions pool


shouldBe' :: (MonadIO m, Eq a, Show a) =>
            a ->
            a ->
            m ()
shouldBe' = (liftIO .) . shouldBe
infix 1 `shouldBe'`


shouldSatisfy' :: (MonadIO m, Show a) =>
                  a ->
                  (a -> Bool) ->
                  m ()
shouldSatisfy' = (liftIO .) . shouldSatisfy
infix 1 `shouldSatisfy'`


shouldApproximatelyBe :: (AEq a, Show a) =>
                         a ->
                         a ->
                         Expectation
shouldApproximatelyBe actual expected =
    unless (actual ~== expected) $
        assertFailure $ "expected: " ++ show expected ++ "\n but got: " ++ show actual
infix 1 `shouldApproximatelyBe`


shouldApproximatelyBe' :: (MonadIO m, AEq a, Show a) =>
                          a ->
                          a ->
                          m ()
shouldApproximatelyBe' = (liftIO .) . shouldApproximatelyBe
infix 1 `shouldApproximatelyBe'`


sha1 :: Monad m =>
        Producer B.ByteString m () ->
        m B.ByteString
sha1 producer = do
    digest <- F.purely P.fold hash producer
    return $ digestToHexByteString (digest :: Digest SHA1)


hash :: HashAlgorithm a =>
        F.Fold B.ByteString (Digest a)
hash = F.Fold hashUpdate hashInit hashFinalize
