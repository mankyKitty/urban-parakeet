{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_rb_vty (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/manky/.cabal/bin"
libdir     = "/home/manky/.cabal/lib/x86_64-linux-ghc-8.0.2/rb-vty-0.1.0.0"
dynlibdir  = "/home/manky/.cabal/lib/x86_64-linux-ghc-8.0.2"
datadir    = "/home/manky/.cabal/share/x86_64-linux-ghc-8.0.2/rb-vty-0.1.0.0"
libexecdir = "/home/manky/.cabal/libexec"
sysconfdir = "/home/manky/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "rb_vty_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "rb_vty_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "rb_vty_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "rb_vty_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "rb_vty_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "rb_vty_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
