{-# LANGUAGE CPP #-}
module LXC
       (
#if linux_HOST_OS
         module System.LXC
#else
         module LXC.Compat
#endif
       ) where
#if osx_HOST_OS
import System.LXC
#else
import LXC.Compat
#endif
