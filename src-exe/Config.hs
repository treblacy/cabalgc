{-# language RecordWildCards #-} -- Rationalization: Small module, few field names. :)

-- | Configuration parameters I need.
module Config where

import System.Directory
import System.Process

-- | Record of program names and directory names I need.
data Config = Config{
    ghcpkg :: String,       -- ^ ghc-pkg
    cabalstore :: String,   -- ^ cabal store directory
    cabaldb :: String       -- ^ cabal store package.db
    }
    deriving Show

-- | Make 'Config' with or without a provided GHC numeric version, and the
-- version number may be incomplete (e.g. 8.10).
--
-- | This procedure asks @ghc --numeric-version@ or
-- @ghc-<ver>--numeric-version@ for the full numeric version, then hands over to
-- 'configVersioned'.
config mver = do
    ver <- (head . lines) `fmap` readProcess ghc ["--numeric-version"] ""
    configVersioned ver
  where
    suffix = case mver of
               Nothing -> ""
               Just v -> "-" ++ v
    ghc = "ghc" ++ suffix

-- | Make 'Config' from full GHC numeric version (e.g. 8.10.7). This is still in
-- IO for obtaining the user's home directory so as to construct the cabal store
-- path.
configVersioned ver = do
    home <- getHomeDirectory
    let ghcpkg = "ghc-pkg-" ++ ver
        cabalstore = home ++ "/.cabal/store/ghc-" ++ ver 
        cabaldb = cabalstore ++ "/package.db"
    return Config{..}

