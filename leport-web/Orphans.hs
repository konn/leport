{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving                                   #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Orphans () where
import           ClassyPrelude                hiding ((<>))
import           Control.Monad.Logger         (MonadLogger (..))
import           Data.Binary                  (Binary (get, put))
import           Language.Haskell.Interpreter (InterpreterT)
import qualified Test.QuickCheck              as QC

deriving instance Typeable QC.Result


instance Binary Text where
  get = pack <$> get
  put = put . unpack


instance MonadLogger m => MonadLogger (InterpreterT m) where
  monadLoggerLog loc src lvl msg = lift $ monadLoggerLog loc src lvl msg

