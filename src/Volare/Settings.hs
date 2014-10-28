module Volare.Settings
    ( PersistConfig
    , development
    , staticDir
    , widgetFile
    ) where

import Data.Default (def)
import Database.Persist.Postgresql (PostgresConf)
import Language.Haskell.TH.Syntax
    ( Exp
    , Q
    )
import Text.Hamlet
    ( NewlineStyle(NoNewlines)
    , defaultHamletSettings
    , hamletNewlines
    )
import Yesod.Default.Util
    ( WidgetFileSettings
    , wfsHamletSettings
    , widgetFileNoReload
    , widgetFileReload
    )


type PersistConfig = PostgresConf


development :: Bool
#if DEVELOPMENT
development = True
#else
development = False
#endif


staticDir :: FilePath
staticDir = if development then
                "static"
            else
                "static_build"


widgetFile :: String ->
              Q Exp
widgetFile = widgetFile' widgetFileSettings
  where
    widgetFile' = if development then
                      widgetFileReload
                  else
                      widgetFileNoReload


widgetFileSettings :: WidgetFileSettings
widgetFileSettings = def {
    wfsHamletSettings = defaultHamletSettings {
        hamletNewlines = NoNewlines
    }
}
