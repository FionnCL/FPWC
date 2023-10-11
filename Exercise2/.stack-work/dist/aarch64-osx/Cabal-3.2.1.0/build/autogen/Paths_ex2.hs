{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_ex2 (
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
version = Version [1,0,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/butrfeld/REPOS/makex2/Exercise2/.stack-work/install/aarch64-osx/447c359600c4da9003d8df3ce7a053c862a55c8254012fbd5c4e09cf55408421/8.10.7/bin"
libdir     = "/Users/butrfeld/REPOS/makex2/Exercise2/.stack-work/install/aarch64-osx/447c359600c4da9003d8df3ce7a053c862a55c8254012fbd5c4e09cf55408421/8.10.7/lib/aarch64-osx-ghc-8.10.7/ex2-1.0.0.0-3L6Phgju5rL2lvpUBHcDwQ"
dynlibdir  = "/Users/butrfeld/REPOS/makex2/Exercise2/.stack-work/install/aarch64-osx/447c359600c4da9003d8df3ce7a053c862a55c8254012fbd5c4e09cf55408421/8.10.7/lib/aarch64-osx-ghc-8.10.7"
datadir    = "/Users/butrfeld/REPOS/makex2/Exercise2/.stack-work/install/aarch64-osx/447c359600c4da9003d8df3ce7a053c862a55c8254012fbd5c4e09cf55408421/8.10.7/share/aarch64-osx-ghc-8.10.7/ex2-1.0.0.0"
libexecdir = "/Users/butrfeld/REPOS/makex2/Exercise2/.stack-work/install/aarch64-osx/447c359600c4da9003d8df3ce7a053c862a55c8254012fbd5c4e09cf55408421/8.10.7/libexec/aarch64-osx-ghc-8.10.7/ex2-1.0.0.0"
sysconfdir = "/Users/butrfeld/REPOS/makex2/Exercise2/.stack-work/install/aarch64-osx/447c359600c4da9003d8df3ce7a053c862a55c8254012fbd5c4e09cf55408421/8.10.7/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "ex2_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "ex2_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "ex2_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "ex2_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "ex2_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "ex2_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
