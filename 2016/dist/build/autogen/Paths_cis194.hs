module Paths_cis194 (
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

bindir     = "/home/rogan/.cabal/bin"
libdir     = "/home/rogan/.cabal/lib/x86_64-linux-ghc-7.10.3/cis194-0.1.0.0-3YD6ofq9qQy9ehmInA9qxL"
datadir    = "/home/rogan/.cabal/share/x86_64-linux-ghc-7.10.3/cis194-0.1.0.0"
libexecdir = "/home/rogan/.cabal/libexec"
sysconfdir = "/home/rogan/.cabal/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "cis194_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "cis194_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "cis194_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "cis194_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "cis194_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
