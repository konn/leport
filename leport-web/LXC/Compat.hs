{-# LANGUAGE DeriveFunctor, EmptyDataDecls, OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving                               #-}
module LXC.Compat (LXC(), withContainer, Container(..)) where
import ClassyPrelude.Yesod

data LXC a = LXC

instance Monad LXC where
  return _ = LXC
  _ >>= _  = LXC
deriving instance Functor LXC
instance Applicative LXC where
  pure  = return
  (<*>) = ap
instance MonadIO LXC where
  liftIO = const LXC

notImpl :: a
notImpl = error "Not implemented"

withContainer :: MonadIO m => Container -> LXC a -> m a
withContainer _ _ = return notImpl

data Container = Container { containerName       :: String
                           , containerConfigPath :: Maybe String
                           } deriving (Show)
