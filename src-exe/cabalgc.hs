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
import qualified PkgTrie

main = do
    Params{ghcver, printFullHash, commitment, pkgIDs, command} <- getParams
    cfg <- config ghcver
    deps <- getDepGraph cfg
    let depGraph = fromAdjs deps
        revDepGraph = transpose depGraph
        -- Those two graphs have only libraries in the cabal store, not
        -- libraries that come with GHC.
        pkgs = PkgTrie.collect (vertices depGraph)
        pkgComplete = PkgTrie.complete pkgs
        pkgShow | printFullHash = id
                | otherwise = PkgTrie.shorten pkgs
    cPkgIDs <- completeOrErrAmb pkgComplete pkgShow pkgIDs
    case command of
      List -> putStr ((unlines . sortCI . map pkgShow . vertices) depGraph)
      ListTops -> putStr ((unlines . sortCI . map pkgShow) indeps)
        where
          indeps = (map fst . filter (null . snd) . toAdjLists) revDepGraph
      ListDeps -> listDeps pkgShow "->" depGraph cPkgIDs
      ListRevDeps -> listDeps pkgShow "<-" revDepGraph cPkgIDs
      GC -> case removeExceptSort depGraph cPkgIDs of
        NotFound ps -> do
            hPutStrLn stderr (unlines
              ("Error: These are not in the cabal store, \
               \no removal for now, \
               \please check for typoes:" : ps))
            exitWith (ExitFailure 1)
        Remove ps -> mapM_ (remove cfg commitment pkgShow) ps
      RM -> do
          mapM_ (remove cfg commitment pkgShow) oRemoves
          mapM_ warnNeeded oNeeds
          mapM_ warnNotFound oNotFound
        where
          RemoveOnly{oRemoves, oNeeds, oNotFound} = removeOnlySort depGraph cPkgIDs
          warnNeeded (p, rs) = hPutStrLn stderr
            ("Warning: " ++ pkgShow p ++ " not removed: needed by: " ++
             (unwords . map pkgShow) rs)
          warnNotFound p = hPutStrLn stderr
            ("Warning: " ++ p ++ " not removed: not in the cabal store.")

data Completions = Completions{
    ccUniq :: [String],
    ccAmb :: [(String, [String])],
    ccNone :: [String]
    }
    deriving Show

completions pkgComplete = foldr op z
  where
    z = Completions{ccUniq=[], ccAmb=[], ccNone=[]}
    op p ~r@Completions{ccUniq, ccAmb, ccNone} = case pkgComplete p of
        [u] -> r{ccUniq = u : ccUniq}
        ps@(_:_:_) -> r{ccAmb =  (p, ps) : ccAmb}
        [] -> r{ccNone = p : ccNone}

-- For now we merge uniq's with none's, the main program already handles none's.
completeOrErrAmb pkgComplete pkgShow ps
  | null ccAmb = pure (ccUniq ++ ccNone)
  | otherwise = do
        mapM_ errAmb ccAmb
        exitWith (ExitFailure 1)
  where
    Completions{ccUniq, ccAmb, ccNone} = completions pkgComplete ps
    errAmb (p, ps) = hPutStrLn stderr
      ("Error: " ++ p ++ " has multiple completions:\n" ++
       (unlines . map (("  " ++) . pkgShow)) ps)

remove _cfg Dryrun pkgShow p =
    putStrLn ("Would remove " ++ pkgShow p)
remove Config{ghcpkg, cabalstore, cabaldb} Doit pkgShow p = do
    putStrLn ("Removing " ++ pkgShow p)
    callProcess ghcpkg args
    removeDirectoryRecursive (cabalstore ++ "/" ++ p)
  where
    args = ["--package-db=" ++ cabaldb,
            "--ipid",
            "unregister",
            p]

listDeps pkgShow arrow g ps = case depRestrict ps g of
    (notFound, g') -> do
        putStr (showGraph pkgShow arrow g')
        mapM_ warnNotFound notFound
  where
    warnNotFound p = hPutStrLn stderr
      ("Warning: " ++ p ++ " omitted: not in the cabal store.")

depRestrict [] g = ([], g)
depRestrict ps g = (notFound, g')
  where
    notFound = [v | v <- ps, not (isVertex v g)]
    g' = subgraph (concatMap flatten (dfsFroms ps g)) g

showGraph pkgShow arrow = concatMap showNode .
                          map (\(p,ps) -> (p, sortCI ps)) .
                          sortCIOn fst .
                          toAdjLists
  where
    showNode (p, ps) = pkgShow p ++ " " ++ arrow ++ "\n" ++
                       concatMap (\x -> indent ++ pkgShow x ++ "\n") ps ++
                       indent ++ ";\n"
    indent = "    "

-- case-insensitive sorts
sortCI = sortOn (map toLower)
sortCIOn f = sortOn (map toLower . f)
