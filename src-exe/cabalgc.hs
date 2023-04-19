{-# language NamedFieldPuns #-}

import Control.Monad (when)
import Data.Char
import Data.List hiding (transpose)
import System.Directory
import System.Exit
import System.IO
import System.Process

import CLI
import Config
import GC
import GetDeps
import MonoGraph

main = do
    Params{ghcver, pkgIDs, commitment, command} <- getParams
    cfg <- config ghcver
    deps <- getDepGraph cfg
    let depGraph = fromAdjs deps
        revDepGraph = transpose depGraph
        -- Those two graphs have only libraries in the cabal store, not
        -- libraries that come with GHC.
    case command of
      List -> putStr ((unlines . sortCI . vertices) depGraph)
      ListTops -> putStr ((unlines . sortCI) indeps)
        where
          indeps = (map fst . filter (null . snd) . toAdjLists) revDepGraph
      ListDeps -> listDeps "->" depGraph pkgIDs
      ListRevDeps -> listDeps "<-" revDepGraph pkgIDs
      GC -> case removeExceptSort depGraph pkgIDs of
        NotFound ps -> do
            hPutStrLn stderr (unlines
              ("Error: These are not in the cabal store, \
               \no removal for now, \
               \please check for typoes:" : ps))
            exitWith (ExitFailure 1)
        Remove ps -> mapM_ (remove cfg commitment) ps
      RM -> do
          mapM_ (remove cfg commitment) oRemoves
          mapM_ warnNeeded oNeeds
          mapM_ warnNotFound oNotFound
        where
          RemoveOnly{oRemoves, oNeeds, oNotFound} = removeOnlySort depGraph pkgIDs
          warnNeeded (p, rs) = hPutStrLn stderr
            ("Warning: " ++ p ++ " not removed: needed by: " ++ unwords rs)
          warnNotFound p = hPutStrLn stderr
            ("Warning: " ++ p ++ " not removed: not in the cabal store.")

remove _cfg Dryrun p =
    putStrLn ("Would remove " ++ p)
remove Config{ghcpkg, cabalstore, cabaldb} Doit p = do
    putStrLn ("Removing " ++ p)
    callProcess ghcpkg args
    removeDirectoryRecursive (cabalstore ++ "/" ++ p)
  where
    args = ["--package-db=" ++ cabaldb,
            "--ipid",
            "unregister",
            p]

listDeps arrow g ps = case depRestrict ps g of
    (notFound, g') -> do
        putStr (showGraph arrow g')
        mapM_ warnNotFound notFound
  where
    warnNotFound p = hPutStrLn stderr
      ("Warning: " ++ p ++ " omitted: not in the cabal store.")

depRestrict [] g = ([], g)
depRestrict ps g = (notFound, g')
  where
    notFound = [v | v <- ps, not (isVertex v g)]
    g' = subgraph (concatMap flatten (dfsFroms ps g)) g

showGraph arrow = concatMap showNode .
                  map (\(p,ps) -> (p, sortCI ps)) .
                  sortCIOn fst .
                  toAdjLists
  where
    showNode (p, ps) = p ++ " " ++ arrow ++ "\n" ++
                       concatMap (\x -> indent ++ x ++ "\n") ps ++
                       indent ++ ";\n"
    indent = "    "

-- case-insensitive sorts
sortCI = sortOn (map toLower)
sortCIOn f = sortOn (map toLower . f)
