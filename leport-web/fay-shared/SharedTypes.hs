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

data Result = Failure [Text]
            | Success Text
              deriving (Eq, Typeable)

data RunReport = RunReport Text (Returns Result)
               deriving (Eq, Typeable)

#ifndef FAY
deriving instance Show Result
deriving instance Read Result
deriving instance Ord  Result
deriving instance Data  Result

deriving instance Show   RunReport
deriving instance Read   RunReport
deriving instance Data   RunReport
#endif
