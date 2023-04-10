{-# language NamedFieldPuns #-}

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
      ListDeps -> putStr (showGraph "->" depGraph)
      ListRevDeps -> putStr (showGraph "<-" revDepGraph)
      GC -> case removalOrder deps pkgIDs of
        NotFound ps -> do
            hPutStrLn stderr (unlines
              ("Error: These are not in the cabal store, \
               \no removal for now, \
               \please check for typoes:" : ps))
            exitWith (ExitFailure 1)
        Remove ps -> mapM_ (remove cfg commitment) ps
      RM ->
        -- Not the perfect algorithm but it works for now. Shortcomings:
        -- Perhaps not an efficient algorithm?
        case removalOrder deps keeps of
          Remove ps -> do
              mapM_ (remove cfg commitment) ps
              mapM_ warnKept (pkgIDs \\ ps)
            where
              warnKept p = hPutStrLn stderr
                ("Warning: " ++ p ++ " not removed: " ++
                 if p `elem` knownPkgs
                 then "needed by other packages."
                 else "did not exist.")
          -- NotFound should not happen here.
          NotFound ps ->
            error (unlines ("Should not happen, but these are \"not found\":" : ps))
        where
          keeps = knownPkgs \\ pkgIDs
          knownPkgs = map fst deps
          -- candidateRevDeps = filter (\(p, _) -> p `elem` pkgIDs) (graphRev graph)

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
