{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_addition (
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

bindir     = "/Users/Work/haskell/haskell-log/14_testing/testing/.stack-work/install/x86_64-osx/cfddb95a639bde76b60272f41417a20ed675344473e3d96e384681950ba7bd65/8.8.4/bin"
libdir     = "/Users/Work/haskell/haskell-log/14_testing/testing/.stack-work/install/x86_64-osx/cfddb95a639bde76b60272f41417a20ed675344473e3d96e384681950ba7bd65/8.8.4/lib/x86_64-osx-ghc-8.8.4/addition-0.1.0.0-FA95gMzNiMPF5kZGRJS0wX"
dynlibdir  = "/Users/Work/haskell/haskell-log/14_testing/testing/.stack-work/install/x86_64-osx/cfddb95a639bde76b60272f41417a20ed675344473e3d96e384681950ba7bd65/8.8.4/lib/x86_64-osx-ghc-8.8.4"
datadir    = "/Users/Work/haskell/haskell-log/14_testing/testing/.stack-work/install/x86_64-osx/cfddb95a639bde76b60272f41417a20ed675344473e3d96e384681950ba7bd65/8.8.4/share/x86_64-osx-ghc-8.8.4/addition-0.1.0.0"
libexecdir = "/Users/Work/haskell/haskell-log/14_testing/testing/.stack-work/install/x86_64-osx/cfddb95a639bde76b60272f41417a20ed675344473e3d96e384681950ba7bd65/8.8.4/libexec/x86_64-osx-ghc-8.8.4/addition-0.1.0.0"
sysconfdir = "/Users/Work/haskell/haskell-log/14_testing/testing/.stack-work/install/x86_64-osx/cfddb95a639bde76b60272f41417a20ed675344473e3d96e384681950ba7bd65/8.8.4/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "addition_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "addition_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "addition_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "addition_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "addition_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "addition_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
