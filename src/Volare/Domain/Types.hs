module Volare.Domain.Types
    ( Query
    , Store
    , Delete) where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Reader (ReaderT)
import qualified Database.Persist as P


type Query model r = forall m backend. ( Functor m
                                       , Applicative m
                                       , Monad m
                                       , MonadIO m
                                       , P.PersistQuery backend
                                       , backend ~ P.PersistEntityBackend model
                                       ) =>
                     ReaderT backend m r

type Store model r = forall m backend. ( Functor m
                                       , Applicative m
                                       , Monad m
                                       , MonadIO m
                                       , P.PersistStore backend
                                       , backend ~ P.PersistEntityBackend model
                                       ) =>
                     ReaderT backend m r

type Delete model r = forall m backend. ( Functor m
                                        , Applicative m
                                        , Monad m
                                        , MonadIO m
                                        , P.PersistStore backend
                                        , backend ~ P.PersistEntityBackend model
                                        ) =>
                      ReaderT backend m r
