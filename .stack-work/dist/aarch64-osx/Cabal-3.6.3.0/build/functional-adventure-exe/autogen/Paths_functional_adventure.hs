{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_functional_adventure (
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
bindir     = "/Users/yvesyang/Documents/School/UChicago/2023Spring/Functional Programming/Homework/Final Project/functional-adventure-final-531Yvonne/.stack-work/install/aarch64-osx/5917f23cf96974d36eba0fa8ae75ffa787d63e7860f366fabe476dd8f0d4b418/9.2.5/bin"
libdir     = "/Users/yvesyang/Documents/School/UChicago/2023Spring/Functional Programming/Homework/Final Project/functional-adventure-final-531Yvonne/.stack-work/install/aarch64-osx/5917f23cf96974d36eba0fa8ae75ffa787d63e7860f366fabe476dd8f0d4b418/9.2.5/lib/aarch64-osx-ghc-9.2.5/functional-adventure-0.1.0.0-D38jgGJMzw46lad8rHj1YI-functional-adventure-exe"
dynlibdir  = "/Users/yvesyang/Documents/School/UChicago/2023Spring/Functional Programming/Homework/Final Project/functional-adventure-final-531Yvonne/.stack-work/install/aarch64-osx/5917f23cf96974d36eba0fa8ae75ffa787d63e7860f366fabe476dd8f0d4b418/9.2.5/lib/aarch64-osx-ghc-9.2.5"
datadir    = "/Users/yvesyang/Documents/School/UChicago/2023Spring/Functional Programming/Homework/Final Project/functional-adventure-final-531Yvonne/.stack-work/install/aarch64-osx/5917f23cf96974d36eba0fa8ae75ffa787d63e7860f366fabe476dd8f0d4b418/9.2.5/share/aarch64-osx-ghc-9.2.5/functional-adventure-0.1.0.0"
libexecdir = "/Users/yvesyang/Documents/School/UChicago/2023Spring/Functional Programming/Homework/Final Project/functional-adventure-final-531Yvonne/.stack-work/install/aarch64-osx/5917f23cf96974d36eba0fa8ae75ffa787d63e7860f366fabe476dd8f0d4b418/9.2.5/libexec/aarch64-osx-ghc-9.2.5/functional-adventure-0.1.0.0"
sysconfdir = "/Users/yvesyang/Documents/School/UChicago/2023Spring/Functional Programming/Homework/Final Project/functional-adventure-final-531Yvonne/.stack-work/install/aarch64-osx/5917f23cf96974d36eba0fa8ae75ffa787d63e7860f366fabe476dd8f0d4b418/9.2.5/etc"

getBinDir     = catchIO (getEnv "functional_adventure_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "functional_adventure_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "functional_adventure_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "functional_adventure_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "functional_adventure_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "functional_adventure_sysconfdir") (\_ -> return sysconfdir)




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
