module SpecUtils (
    runDB,
    (@==)
) where

import Control.Applicative ((<*))
import Control.Monad.IO.Class (MonadIO,
                               liftIO)
import Control.Monad.Logger (NoLoggingT,
                             runNoLoggingT)
import Control.Monad.Trans.Resource (ResourceT,
                                     runResourceT)
import Database.Persist.Class (PersistConfigBackend,
                               applyEnv,
                               createPoolConfig,
                               loadConfig,
                               runPool)
import Database.Persist.Sql (runMigration,
                             transactionUndo)
import Test.HUnit ((@?=))
import Yesod.Default.Config (DefaultEnv(Testing),
                             withYamlEnvironment)

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
