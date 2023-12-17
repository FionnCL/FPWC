{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_prfchk (
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

bindir     = "C:\\Users\\camac\\Desktop\\FPCW\\proof-check\\.stack-work\\install\\659f3b9f\\bin"
libdir     = "C:\\Users\\camac\\Desktop\\FPCW\\proof-check\\.stack-work\\install\\659f3b9f\\lib\\x86_64-windows-ghc-8.10.7\\prfchk-1.0.0.0-C4254jxdc4KjOSS0kU5W0"
dynlibdir  = "C:\\Users\\camac\\Desktop\\FPCW\\proof-check\\.stack-work\\install\\659f3b9f\\lib\\x86_64-windows-ghc-8.10.7"
datadir    = "C:\\Users\\camac\\Desktop\\FPCW\\proof-check\\.stack-work\\install\\659f3b9f\\share\\x86_64-windows-ghc-8.10.7\\prfchk-1.0.0.0"
libexecdir = "C:\\Users\\camac\\Desktop\\FPCW\\proof-check\\.stack-work\\install\\659f3b9f\\libexec\\x86_64-windows-ghc-8.10.7\\prfchk-1.0.0.0"
sysconfdir = "C:\\Users\\camac\\Desktop\\FPCW\\proof-check\\.stack-work\\install\\659f3b9f\\etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "prfchk_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "prfchk_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "prfchk_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "prfchk_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "prfchk_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "prfchk_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
