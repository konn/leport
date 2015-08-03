{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, StandaloneDeriving, OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Orphans () where
import           ClassyPrelude hiding ((<>))
import Data.Monoid ((<>))
import           Control.Distributed.Process                  (Process)
import qualified Control.Distributed.Process                  as CH
import           Control.Distributed.Process.MonadBaseControl ()
import qualified Control.Exception.Lifted                     as LE
import qualified Control.Monad.Catch                          as MC
import           Control.Monad.Logger                         (MonadLogger (..))
import           Language.Haskell.TH.Syntax                   (Loc (..))
import qualified Test.QuickCheck                              as QC
import Control.Monad.Logger (toLogStr)
import System.Log.FastLogger (fromLogStr)
import Language.Haskell.Interpreter (InterpreterT)
import Data.Binary (Binary(get,put))

deriving instance Typeable QC.Result


instance Binary Text where
  get = pack <$> get
  put = put . unpack

instance MonadThrow Process where
  throwM = throwIO

instance MC.MonadCatch Process where
  catch = CH.catch

instance MonadMask Process where
  mask = LE.mask
  uninterruptibleMask = LE.uninterruptibleMask

instance MonadLogger m => MonadLogger (InterpreterT m) where
  monadLoggerLog loc src lvl msg = lift $ monadLoggerLog loc src lvl msg

instance MonadLogger Process where
  monadLoggerLog loc src lvl msg =
    CH.say $ unpack $ decodeUtf8 $ fromLogStr $ 
    showLoc loc <> " from " <> toLogStr src <> " [" <> toLogStr (show lvl) <> "] "
    <> toLogStr msg

showLoc Loc{..} =
  mconcat [ toLogStr loc_module, ":", toLogStr (show $ fst loc_start), ":"
          , toLogStr (show $ snd loc_start)
          , " (in " <> toLogStr loc_filename <> ", from" <> toLogStr loc_package <> "package ) "
          ]
