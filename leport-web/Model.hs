{-# LANGUAGE FlexibleInstances #-}
module Model where

import ClassyPrelude.Yesod
import Database.Persist.Quasi
import Types
import Lens
import Yesod.Auth.HashDB (HashDBUser(..))
import Control.Lens ((?~))

-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
share [mkPersist sqlSettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models")

persistMakeClassy ''User
persistMakeClassy ''Report
persistMakeClassy ''Answer

instance HashDBUser User where
  userPasswordHash  = userPassword
  setPasswordHash h = _userPassword ?~ h

