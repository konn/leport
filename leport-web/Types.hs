{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings, DeriveGeneric, StandaloneDeriving #-}
{-# LANGUAGE StandaloneDeriving                                   #-}
module Types where
import ClassyPrelude
import Data.Binary (Binary)
import Orphans ()
import ClassyPrelude.Yesod
import Data.Aeson          (decode, encode)
import Data.Data           (Data)
import Data.Maybe          (fromJust)
import Fay.Convert         (readFromFay, showToFay)
import Network.WebSockets  (WebSocketsData (..))
import Orphans             ()

data Result a = Failure [Text]
              | Success a
              deriving (Eq, Typeable)

data ReportCommand = Single   Text
                   | Multiple
                     deriving (Eq, Typeable)


data ReportEvent = CheckResult { function   :: Text
                               , succeeded  :: Bool
                               , ghcMessage :: Text
                               }
                 | Information [Text]
                 | Finished
                 | Exception [Text]
                 deriving (Eq, Typeable)

deriving instance Show a => Show (Result a)
deriving instance Read a => Read (Result a)
deriving instance Ord a => Ord (Result a)
deriving instance Data a => Data (Result a)

deriving instance Show ReportEvent
deriving instance Read ReportEvent
deriving instance Data ReportEvent
deriving instance Generic ReportEvent

instance Binary ReportEvent

deriving instance Show ReportCommand
deriving instance Read ReportCommand
deriving instance Data ReportCommand

instance WebSocketsData ReportCommand where
  fromLazyByteString = fromJust . (readFromFay <=< decode)
  toLazyByteString = encode . showToFay

instance WebSocketsData ReportEvent where
  fromLazyByteString = fromJust . (readFromFay <=< decode)
  toLazyByteString = encode . showToFay


data Access = Normal
            | Admin
              deriving (Read, Show, Eq, Ord)

derivePersistField "Access"
