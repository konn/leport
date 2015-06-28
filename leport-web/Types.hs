{-# LANGUAGE OverloadedStrings #-}
module Types where
import ClassyPrelude
import ClassyPrelude.Yesod (derivePersistField)

data Access = Normal
            | Admin
              deriving (Read, Show, Eq, Ord)

derivePersistField "Access"
