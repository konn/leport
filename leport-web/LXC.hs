{-# LANGUAGE CPP #-}
module LXC
       (
#if linux_HOST_OS
         module System.LXC
#else
         module LXC.Compat
#endif
       ) where
#if darwin_HOST_OS
import LXC.Compat
#else
import System.LXC
#endif
