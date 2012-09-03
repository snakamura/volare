{-# LANGUAGE OverloadedStrings #-}

module Volare.Config (
    Config(..),
    loadConfig
) where

import Control.Applicative ((<$>),
                            (<*>))
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
loadConfig path = do
  config <- decodeFile path
  case config of
    Just config -> return config
    Nothing -> error "Failed to parse a config file."
