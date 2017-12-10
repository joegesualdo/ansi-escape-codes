{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_ansi_escape_codes (
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

bindir     = "/Users/jgesualdo/code/haskell/ansi-escape-codes/.cabal-sandbox/bin"
libdir     = "/Users/jgesualdo/code/haskell/ansi-escape-codes/.cabal-sandbox/lib/x86_64-osx-ghc-8.2.1/ansi-escape-codes-0.1.0.0-8Wn24kROLZQ9A8aV1oLVd2"
dynlibdir  = "/Users/jgesualdo/code/haskell/ansi-escape-codes/.cabal-sandbox/lib/x86_64-osx-ghc-8.2.1"
datadir    = "/Users/jgesualdo/code/haskell/ansi-escape-codes/.cabal-sandbox/share/x86_64-osx-ghc-8.2.1/ansi-escape-codes-0.1.0.0"
libexecdir = "/Users/jgesualdo/code/haskell/ansi-escape-codes/.cabal-sandbox/libexec/x86_64-osx-ghc-8.2.1/ansi-escape-codes-0.1.0.0"
sysconfdir = "/Users/jgesualdo/code/haskell/ansi-escape-codes/.cabal-sandbox/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "ansi_escape_codes_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "ansi_escape_codes_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "ansi_escape_codes_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "ansi_escape_codes_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "ansi_escape_codes_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "ansi_escape_codes_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
