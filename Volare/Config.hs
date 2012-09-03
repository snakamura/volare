{-# LANGUAGE OverloadedStrings #-}

module Volare.Config (
    Config(..),
    loadConfig
) where

import Control.Applicative ((<$>),
                            (<*>))
import Control.Monad ((>=>))
import Data.Monoid (mempty)
import Data.Yaml (FromJSON(parseJSON),
                  Value(Object),
                  (.:),
                  decodeFile)
import qualified Data.Text as T


data Config = Config {
      sqlitePath :: T.Text,
      sqliteConnectionPoolCount :: Int
    } deriving Show


instance FromJSON Config where
    parseJSON (Object o) = Config <$> o .: "sqlitePath"
                                  <*> o .: "sqliteConnectionPoolCount"
    parseJSON _ = mempty


loadConfig :: FilePath ->
              IO Config
loadConfig = decodeFile >=> maybe (error "Failed to parse a config file.") return
