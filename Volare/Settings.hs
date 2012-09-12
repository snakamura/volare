module Volare.Settings (
    development,
    staticDir,
    widgetFile
) where

import Data.Default (def)
import Language.Haskell.TH.Syntax (Exp,
                                   Q)
import Text.Hamlet (NewlineStyle(NoNewlines),
                    defaultHamletSettings,
                    hamletNewlines)
import Yesod.Default.Util (WidgetFileSettings,
                           wfsHamletSettings,
                           widgetFileNoReload,
                           widgetFileReload)


development :: Bool
development = True


staticDir :: FilePath
staticDir = "static"


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
