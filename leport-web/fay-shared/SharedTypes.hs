{-# LANGUAGE DeriveDataTypeable, StandaloneDeriving #-}
module SharedTypes where
import Prelude
import Fay.Text
import Data.Data     (Data, Typeable)
import Fay.Yesod     (Returns)
#ifdef FAY
import FFI
#else
import Fay.FFI
#endif

data Result a = Failure [Text]
              | Success a
              deriving (Eq, Typeable)

data FayCommand = RunReport Int Text (Returns (Result Text))
                deriving (Eq, Typeable)

data FayEvent = CheckResult { function   :: Text
                            , succeeded  :: Bool
                            , ghcMessage :: Text
                            }
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

deriving instance Show FayEvent
deriving instance Read FayEvent
deriving instance Data FayEvent
#endif
