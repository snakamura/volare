module Volare.Config (
    Config(..),
    parseConfig
) where

import Control.Applicative ((<$>))
import qualified Data.Text as T
import Data.Yaml (Object,
                  Parser,
                  (.:))


data Config = Config {
    googleApiKey :: T.Text
} deriving Show


parseConfig :: Object ->
               Parser Config
parseConfig o = Config <$> o .: "googleApiKey"
