module Paths_constraints (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
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
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "F:\\Dropbox\\Projects\\Haskell\\constraints\\.cabal-sandbox\\bin"
libdir     = "F:\\Dropbox\\Projects\\Haskell\\constraints\\.cabal-sandbox\\x86_64-windows-ghc-7.8.3\\constraints-0.1.0.0"
datadir    = "F:\\Dropbox\\Projects\\Haskell\\constraints\\.cabal-sandbox\\x86_64-windows-ghc-7.8.3\\constraints-0.1.0.0"
libexecdir = "F:\\Dropbox\\Projects\\Haskell\\constraints\\.cabal-sandbox\\constraints-0.1.0.0"
sysconfdir = "F:\\Dropbox\\Projects\\Haskell\\constraints\\.cabal-sandbox\\etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "constraints_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "constraints_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "constraints_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "constraints_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "constraints_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
