{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_makex4 (
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

bindir     = "/Users/butrfeld/Desktop/makex4/.stack-work/install/aarch64-osx/447c359600c4da9003d8df3ce7a053c862a55c8254012fbd5c4e09cf55408421/8.10.7/bin"
libdir     = "/Users/butrfeld/Desktop/makex4/.stack-work/install/aarch64-osx/447c359600c4da9003d8df3ce7a053c862a55c8254012fbd5c4e09cf55408421/8.10.7/lib/aarch64-osx-ghc-8.10.7/makex4-0.1.0.0-HCrlDejvc4hCxnIjz5HMoH-makex4"
dynlibdir  = "/Users/butrfeld/Desktop/makex4/.stack-work/install/aarch64-osx/447c359600c4da9003d8df3ce7a053c862a55c8254012fbd5c4e09cf55408421/8.10.7/lib/aarch64-osx-ghc-8.10.7"
datadir    = "/Users/butrfeld/Desktop/makex4/.stack-work/install/aarch64-osx/447c359600c4da9003d8df3ce7a053c862a55c8254012fbd5c4e09cf55408421/8.10.7/share/aarch64-osx-ghc-8.10.7/makex4-0.1.0.0"
libexecdir = "/Users/butrfeld/Desktop/makex4/.stack-work/install/aarch64-osx/447c359600c4da9003d8df3ce7a053c862a55c8254012fbd5c4e09cf55408421/8.10.7/libexec/aarch64-osx-ghc-8.10.7/makex4-0.1.0.0"
sysconfdir = "/Users/butrfeld/Desktop/makex4/.stack-work/install/aarch64-osx/447c359600c4da9003d8df3ce7a053c862a55c8254012fbd5c4e09cf55408421/8.10.7/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "makex4_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "makex4_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "makex4_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "makex4_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "makex4_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "makex4_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
