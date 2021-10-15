{-# language RecordWildCards #-} -- Rationalization: Small module, few field names. :)

-- | Read and construct the dependency graph of the cabal store.
module GetDeps(getDepGraph, graphRev, graphTrim) where

import           Control.Monad
import           Data.List
import qualified Data.Map.Strict as Map
import           System.Process
import           Text.Parsec
import           Text.Parsec.Char
import           Text.Parsec.String

import           Config

-- | Get the dependency graph of the cabal store.
--
-- The graph is in terms of [list of] adjacency lists: A tuple (x, [y,z]) means
-- that x depends on y and z.
getDepGraph :: Config -> IO [(String, [String])]
getDepGraph Config{..} = do
    dump <- readProcess ghcpkg args ""
    case parse pkgGraph "ghc-pkg output" dump of
      Left e -> fail (show e)
      Right g -> pure g
  where
    args = ["--package-db=" ++ cabaldb, "--expand-pkgroot", "dump"]

-- | The graph obtained from 'getDepGraph' includes references to what comes
-- with GHC too. This function gets rid of them. Mathematically, in each
-- (x, [y,z,p]), if p is not in the graph, get rid of it.
graphTrim :: Eq a => [(a, [a])] -> [(a, [a])]
graphTrim g = map (\(p,ps) -> (p, filter (`elem` vertices) ps)) g
  where
    vertices = map fst g

-- | Reverse/Transpose the graph.
--
-- If the original graph has (x, [y,z,p]) where p is not in the graph, p will be
-- dropped.
graphRev :: Ord a => [(a, [a])] -> [(a, [a])]
graphRev g = Map.toList dict_final
  where
    dict_0 = Map.fromList (map (\(p, _) -> (p, [])) g)
    dict_final = foldl' outer dict_0 g
    outer dict (u, vs) = foldl' inner dict vs
      where
        inner dict v = Map.adjust (u :) v dict

-- Going out of my way to parse "ghc-pkg dump" output. :)

spc :: Parser Char
spc = char ' '

keyval :: Parser (String, String)
keyval = do
    key <- do
        c <- letter
        cs <- many (alphaNum <|> char '-')
        char ':'
        pure (c:cs)
    s <- spc <|> newline
    val <- if s == ' ' then
             do spaces
                manyTill anyChar newline
           else
             do indent <- many1 spc
                fstLine <- manyTill anyChar newline
                moreLines <- many ((newline >> pure "")
                                   <|>
                                   (string indent >> manyTill anyChar newline))
                -- Strictly speaking, if last moreLines == "", should get rid of it.
                pure (unlines (fstLine : moreLines))
    pure (key, val)

pkg :: Parser [(String, String)]
pkg = many1 keyval

pkgNode :: Parser (String, [String])
pkgNode = do
    mp <- extract <$> pkg
    case mp of
      Nothing -> unexpected "parse error on pkgid or depends"
      Just p -> pure p
  where
    extract dict = do
        i:_ <- words <$> lookup "id" dict
        d <- (words <$> lookup "depends" dict) `mplus` pure []
        pure (i, d)

pkgGraph :: Parser [(String, [String])]
pkgGraph = sepBy pkgNode (try (string "---\n"))
