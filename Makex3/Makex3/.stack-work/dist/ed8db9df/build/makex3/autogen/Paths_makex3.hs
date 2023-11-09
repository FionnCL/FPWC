{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_makex3 (
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

bindir     = "C:\\Users\\camac\\Desktop\\FPCW\\Makex3\\Makex3\\.stack-work\\install\\659f3b9f\\bin"
libdir     = "C:\\Users\\camac\\Desktop\\FPCW\\Makex3\\Makex3\\.stack-work\\install\\659f3b9f\\lib\\x86_64-windows-ghc-8.10.7\\makex3-0.1.0.0-4giJsRNYLBl2ZgcksQ74mf-makex3"
dynlibdir  = "C:\\Users\\camac\\Desktop\\FPCW\\Makex3\\Makex3\\.stack-work\\install\\659f3b9f\\lib\\x86_64-windows-ghc-8.10.7"
datadir    = "C:\\Users\\camac\\Desktop\\FPCW\\Makex3\\Makex3\\.stack-work\\install\\659f3b9f\\share\\x86_64-windows-ghc-8.10.7\\makex3-0.1.0.0"
libexecdir = "C:\\Users\\camac\\Desktop\\FPCW\\Makex3\\Makex3\\.stack-work\\install\\659f3b9f\\libexec\\x86_64-windows-ghc-8.10.7\\makex3-0.1.0.0"
sysconfdir = "C:\\Users\\camac\\Desktop\\FPCW\\Makex3\\Makex3\\.stack-work\\install\\659f3b9f\\etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "makex3_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "makex3_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "makex3_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "makex3_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "makex3_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "makex3_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
