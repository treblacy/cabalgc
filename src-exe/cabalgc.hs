{-# language RecordWildCards #-} -- Rationalization: Small module, few field names. :)

import Data.List
import Data.Char
import System.Directory
import System.Exit
import System.IO
import System.Process

import CLI
import Config
import GC
import GetDeps

main = do
    Params{..} <- getParams
    cfg <- config ghcver
    graph <- getDepGraph cfg
    case command of
      List -> putStr ((unlines . sortOn (map toLower) . map fst) graph)
      GC -> case removalOrder graph keeps of
        NotFound ps -> do
            hPutStrLn stderr (unlines
              ("Error: These are not in the cabal store, \
               \no removal for now, \
               \please check for typoes:" : ps))
            exitWith (ExitFailure 1)
        Remove ps -> mapM_ (remove cfg commitment) ps

remove _cfg Dryrun p =
    putStrLn ("Would remove " ++ p)
remove Config{..} Doit p = do
    putStrLn ("Removing " ++ p)
    callProcess ghcpkg args
    removeDirectoryRecursive (cabalstore ++ "/" ++ p)
  where
    args = ["--package-db=" ++ cabaldb,
            "--ipid",
            "unregister",
            p]

