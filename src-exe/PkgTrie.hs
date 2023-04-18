-- | Support for prefix-based shortening and completion of package IDs.
--
-- Clearly, some kind of trie is involved. :)
module PkgTrie where

import           Data.Char
import           Data.Foldable
import qualified Data.List as List

-- | The Trie type.
data Trie = Trie [(Char, Trie)]
    deriving Show
-- Future plan: Change to compressed trie.

empty :: Trie
empty = Trie []

insert :: Trie -> String -> Trie
insert = undefined

-- | Make a trie of the given members.
collect :: Foldable f => f String -> Trie
collect = foldl' insert empty

-- | Find all members with the given prefix.
complete :: Trie -> String -> [String]
complete = undefined

-- | Find shortest unique prefix of a member.
--
-- Unspecified behaviour if the input is not a member.
shortest :: Trie -> String -> String
shortest = undefined

-- | Canonical shortening of a package ID.  Inspired by git convention,
-- this uses at least 7 hexadecimal digits of the hash part.
shorten :: Trie -> String -> String
shorten = undefined


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
