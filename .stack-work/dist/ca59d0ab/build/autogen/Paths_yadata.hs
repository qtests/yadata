{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_yadata (
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

bindir     = "C:\\hsk\\zreq\\yadata\\.stack-work\\install\\4938d489\\bin"
libdir     = "C:\\hsk\\zreq\\yadata\\.stack-work\\install\\4938d489\\lib\\x86_64-windows-ghc-8.0.2\\yadata-0.1.0.0-5LhR3YabamACloRwT6Hlqc"
dynlibdir  = "C:\\hsk\\zreq\\yadata\\.stack-work\\install\\4938d489\\lib\\x86_64-windows-ghc-8.0.2"
datadir    = "C:\\hsk\\zreq\\yadata\\.stack-work\\install\\4938d489\\share\\x86_64-windows-ghc-8.0.2\\yadata-0.1.0.0"
libexecdir = "C:\\hsk\\zreq\\yadata\\.stack-work\\install\\4938d489\\libexec"
sysconfdir = "C:\\hsk\\zreq\\yadata\\.stack-work\\install\\4938d489\\etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "yadata_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "yadata_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "yadata_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "yadata_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "yadata_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "yadata_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
