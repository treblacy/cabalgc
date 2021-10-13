-- | Some kind of garbage-collection algorithm: Given an acyclic dependency
-- graph and which nodes the user wants to keep, compute a safe removal order of
-- removable nodes.
module GC where

import Data.Foldable
import Data.Graph
import Data.Maybe

data Answer p = NotFound [p] | Remove [p]
    deriving Show

-- | Compute a safe removal order of removable nodes.
--
-- 1st param: The acyclic dependency graph in terms of [list of] adjacency
-- lists.  E.g., a tuple (x, [y,z]) means that x depends on y and z.
--
-- 2nd param: Which nodes the user wants to keep. These and their transitive
-- dependencies will not be in the safe removal order.
--
-- If some nodes in the 2nd list are not in the graph, the answer is NotFound
-- with those elements. This usually happens due to user typoes. We would rather
-- warn the user of possible typoes than proceed with "so, nothing to keep,
-- remove everything"!
--
-- Otherwise, the answer is Remove with a safe removal order.  E.g., if x
-- depends on y and z, then x is earlier in the list, y and z are later, i.e.,
-- topological sort order.
removalOrder :: Ord p => [(p, [p])] -> [p] -> Answer p
removalOrder deps keeps
  | null notfoundKeeps = Remove (map ((\(_,p,_) -> p) . nodeFromV) removes)
  | otherwise = NotFound notfoundKeeps
  where
    (graph, nodeFromV, vFromKey) = graphFromEdges (map (\(p,ps) -> ((),p,ps)) deps)
    -- checkedKeeps is a list like this, if keeps=[x,y]:
    -- [(x, Just 5), (y, Nothing)]
    -- It means that y is not in deps, x is vertex #5 in graph.
    checkedKeeps = map (\p -> (p, vFromKey p)) keeps
    notfoundKeeps = (map fst . filter (isNothing . snd)) checkedKeeps
    moreKeeps = (concatMap toList . dfs graph . catMaybes . map snd) checkedKeeps
    removes = filter (`notElem` moreKeeps) (topSort graph)


-- Some more kinds of requests and algorithms.

-- Remove X = {x1, ..., xn} and transitive dependencies, as much as removable:
-- 1. Compute R = the root nodes (nodes without incoming edges):
--    Start with set of all nodes, for each adjacency list (?, [u, v, w]),
--    delete u, v, w.
-- 2. So we just want to keep R-X and transitive dependencies! Give that to the above. :)


-- Remove X = {x1, ..., xn} and reverse transitive dependencies:
-- 1. Compute G' = transpose of G
-- 2. S = nodesOf (spanning-forest G' X) = nodesOf (dfs G' X)
-- 3. Remove these in order: filter (\v -> v in S) (topSort G)
