{-# LANGUAGE NoImplicitPrelude, PatternGuards #-}
module Main where
import ClassyPrelude
import Control.Distributed.Process.Backend.SimpleLocalnet
import RPC                                                (remoteTable)

main :: IO ()
main = do
  args <- getArgs
  let (host, port)
        | [p] <- args = ("localhost", p)
        | [h, p] <- args = (h, p)
  be <- initializeBackend (unpack host) (unpack port) remoteTable
  startSlave be
