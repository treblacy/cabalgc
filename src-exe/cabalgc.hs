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
      ListTops -> putStr ((unlines . sortOn (map toLower)) indeps)
        where
          indeps = (map fst . filter (null . snd) . graphRev) graph
      ListDeps -> putStr (showGraph "->" sorted)
        where
          sorted = (map (\(p,ps) -> (p, sortOn (map toLower) ps)) .
                    sortOn (map toLower . fst) .
                    graphTrim
                   )
                   graph
      ListRevDeps -> putStr (showGraph "<-" sorted)
        where
          sorted = (map (\(p,ps) -> (p, sortOn (map toLower) ps)) .
                    sortOn (map toLower . fst) .
                    graphRev
                   )
                   graph
      GC -> case removalOrder graph pkgIDs of
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
        case removalOrder graph keeps of
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
          knownPkgs = map fst graph
          -- candidateRevDeps = filter (\(p, _) -> p `elem` pkgIDs) (graphRev graph)

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

showGraph arrow g = concatMap showNode g
  where
    -- showNode (p, []) = p ++ " " ++ arrow ++ " ;\n"
    showNode (p, ps) = p ++ " " ++ arrow ++ "\n" ++
                       concatMap (\x -> indent ++ x ++ "\n") ps ++
                       indent ++ ";\n"
    indent = "    "
