{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_MES (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where


import qualified Control.Exception as Exception
import qualified Data.List as List
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

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath



bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/home/benjamin/.cabal/bin"
libdir     = "/home/benjamin/.cabal/lib/x86_64-linux-ghc-9.2.4/MES-0.1.0.0-inplace-MES"
dynlibdir  = "/home/benjamin/.cabal/lib/x86_64-linux-ghc-9.2.4"
datadir    = "/home/benjamin/.cabal/share/x86_64-linux-ghc-9.2.4/MES-0.1.0.0"
libexecdir = "/home/benjamin/.cabal/libexec/x86_64-linux-ghc-9.2.4/MES-0.1.0.0"
sysconfdir = "/home/benjamin/.cabal/etc"

getBinDir     = catchIO (getEnv "MES_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "MES_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "MES_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "MES_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "MES_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "MES_sysconfdir") (\_ -> return sysconfdir)




joinFileName :: String -> String -> FilePath
joinFileName ""  fname = fname
joinFileName "." fname = fname
joinFileName dir ""    = dir
joinFileName dir fname
  | isPathSeparator (List.last dir) = dir ++ fname
  | otherwise                       = dir ++ pathSeparator : fname

pathSeparator :: Char
pathSeparator = '/'

isPathSeparator :: Char -> Bool
isPathSeparator c = c == '/'
