{-# language RecordWildCards #-} -- Rationalization: Small module, few field names. :)

-- | Read and construct the dependency graph of the cabal store.
module GetDeps(getDepGraph) where

-- I don't export the intermediate procedures---very gory implementation
-- details.

import Control.Monad
import System.Process
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.String

import Config

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
