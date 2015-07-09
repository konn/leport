{-# LANGUAGE DeriveDataTypeable, StandaloneDeriving #-}
module SharedTypes where
import Prelude
import Fay.Text
import Data.Data     (Data, Typeable)
import Fay.Yesod     (Returns)
#ifdef FAY
import FFI
#else
import Network.WebSockets (WebSocketsData(..))
import Fay.FFI
import Fay.Convert (readFromFay, showToFay)
import Data.Aeson (decode, encode)
import Data.Maybe (fromJust)
import Control.Monad ((<=<))
#endif

data Result a = Failure [Text]
              | Success a
              deriving (Eq, Typeable)

data FayCommand = RunReport Int Text (Returns (Result Text))
                | UpdateSpec Text (Returns (Result Text))
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

#ifndef FAY
deriving instance Show a => Show (Result a)
deriving instance Read a => Read (Result a)
deriving instance Ord a => Ord (Result a)
deriving instance Data a => Data (Result a)

deriving instance Show   FayCommand
deriving instance Read   FayCommand
deriving instance Data   FayCommand

deriving instance Show ReportEvent
deriving instance Read ReportEvent
deriving instance Data ReportEvent

deriving instance Show ReportCommand
deriving instance Read ReportCommand
deriving instance Data ReportCommand

instance WebSocketsData ReportCommand where
  fromLazyByteString = fromJust . (readFromFay <=< decode)
  toLazyByteString = encode . showToFay

instance WebSocketsData ReportEvent where
  fromLazyByteString = fromJust . (readFromFay <=< decode)
  toLazyByteString = encode . showToFay
#endif
