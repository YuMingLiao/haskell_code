{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_frontend (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/demo/.cabal/bin"
libdir     = "/home/demo/.cabal/lib/x86_64-linux-ghcjs-0.2.1-ghc8_0_2/frontend-0.1.0.0-inplace-frontend"
dynlibdir  = "/home/demo/.cabal/lib/x86_64-linux-ghcjs-0.2.1-ghc8_0_2"
datadir    = "/home/demo/.cabal/share/x86_64-linux-ghcjs-0.2.1-ghc8_0_2/frontend-0.1.0.0"
libexecdir = "/home/demo/.cabal/libexec/x86_64-linux-ghcjs-0.2.1-ghc8_0_2/frontend-0.1.0.0"
sysconfdir = "/home/demo/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "frontend_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "frontend_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "frontend_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "frontend_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "frontend_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "frontend_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
