{-# LANGUAGE OverloadedStrings,
             TemplateHaskell #-}

module Volare.Config (
    Config(..),
    loadConfig
) where

import Control.Monad ((>=>))
import Data.Aeson.TH (deriveFromJSON)
import Data.Yaml (decodeFile)
import qualified Data.Text as T


data Config = Config {
      sqlitePath :: T.Text,
      sqliteConnectionPoolCount :: Int,
      googleApiKey :: T.Text
    } deriving Show

deriveFromJSON id ''Config


loadConfig :: FilePath ->
              IO Config
loadConfig = decodeFile >=> maybe (error "Failed to parse a config file.") return
