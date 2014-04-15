module Volare.Model.Settings (settings) where

import Database.Persist.Class (entityIdFromJSON,
                               entityIdToJSON)
import Database.Persist.TH (EntityJSON(EntityJSON),
                            MkPersistSettings,
                            entityFromJSON,
                            entityToJSON,
                            mpsEntityJSON,
                            sqlSettings)

settings :: MkPersistSettings
settings = sqlSettings {
    mpsEntityJSON = Just EntityJSON {
      entityToJSON = 'entityIdToJSON,
      entityFromJSON = 'entityIdFromJSON
    }
}
