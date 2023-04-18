-- | Support for prefix-based shortening and completion of package IDs.
--
-- Clearly, some kind of trie is involved. :)
module PkgTrie(Trie, collect, enumerate, complete, shorten, stemAndHash) where

import           Data.Char
import           Data.Foldable
import qualified Data.List as List

-- | The Trie type.
newtype Trie = Trie [(Char, Trie)]
    deriving Show
-- Simplifying assumption: No member is a prefix of another member. (In
-- particular the empty string is never a member.)

-- Future plan: Kill laziness.
-- Future plan: Change to compressed trie.

empty :: Trie
empty = Trie []

singleton :: String -> Trie
singleton = foldr (\c t -> Trie [(c, t)]) empty

insert :: Trie -> String -> Trie
insert t "" = t
insert (Trie children) (c:cs) = case List.lookup c children of
    Nothing -> Trie ((c, singleton cs) : children)
    Just child -> Trie ((c, insert child cs) : (del c children))

del c [] = []
del c (p@(c', _) : cs) | c == c' = cs
                       | otherwise = p : del c cs

-- | Make a trie of the given members.
collect :: Foldable f => f String -> Trie
collect = foldl' insert empty

-- | Enumerate all memebers.
enumerate :: Trie -> [String]
enumerate (Trie []) = [""]
enumerate (Trie children) = [c:cs | (c, t) <- children, cs <- enumerate t]

-- | Find all members with the given prefix.
complete :: Trie -> String -> [String]
complete t "" = enumerate t
complete (Trie children) (c:cs) = case List.lookup c children of
    Nothing -> []
    Just child -> map (c:) (complete child cs)

-- | Canonical shortening of a package ID.  Inspired by git convention, this
-- uses at least 7 hexadecimal digits of the hash part.  It also checks unique
-- completion in the trie and adds more digits if necessary.
--
-- Unspecified behaviour if the pkgID is not a member of the trie.
shorten :: Trie -> String -> String
shorten trie pkgID = case stemAndHash pkgID of
    (stem, hash) -> head [candidate | hs <- drop 7 (List.inits hash)
                                    , let candidate = stem ++ hs
                                    , case complete trie candidate of
                                        [_] -> True
                                        _ -> False
                                    ]

-- | Split a package ID into "stem" (everything before hash, including the last
-- dash) and hash.
--
-- Hash detection heuristic: maximal hexadecimal string at the end, preceded by
-- a dash.
--
-- If no hash is detected, the hash part is empty; see the base example below.
--
-- >>> stemAndHash "awesome-prelude-ultra-1.2.3.4-0123456789abcdef"
-- ("awesome-prelude-ultra-1.2.3.4-","0123456789abcdef")
--
-- >>> stemAndHash "base-4.18.0.0"
-- ("base-4.18.0.0","")
stemAndHash pkgID = case span isHexDigit r of
    (rh, rs) | not (null rh), "-" `List.isPrefixOf` rs -> (reverse rs, reverse rh)
    _ -> (pkgID, "")
  where
    r = reverse pkgID
