{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_servant_reflex (
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
version = Version [0,3,3] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/demo/.cabal/bin"
libdir     = "/home/demo/.cabal/lib/x86_64-linux-ghc-8.0.2/servant-reflex-0.3.3-inplace"
dynlibdir  = "/home/demo/.cabal/lib/x86_64-linux-ghc-8.0.2"
datadir    = "/home/demo/.cabal/share/x86_64-linux-ghc-8.0.2/servant-reflex-0.3.3"
libexecdir = "/home/demo/.cabal/libexec/x86_64-linux-ghc-8.0.2/servant-reflex-0.3.3"
sysconfdir = "/home/demo/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "servant_reflex_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "servant_reflex_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "servant_reflex_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "servant_reflex_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "servant_reflex_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "servant_reflex_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
