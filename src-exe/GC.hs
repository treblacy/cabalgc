{-# language FlexibleContexts #-}
{-# language NamedFieldPuns #-}

-- | Some kind of garbage-collection algorithm: Given an acyclic dependency
-- graph and which nodes the user wants to keep, compute a safe removal order of
-- removable nodes.
module GC where

import Control.Monad.State.Lazy
import Data.Either (partitionEithers)
import Data.Tree

import MonoGraph

-- | Answer type of removeExcept below.
data RemoveExcept p = NotFound [p]  -- ^ nodes not found
                    | Remove [p]    -- ^ nodes in safe removal order
    deriving Show

-- | Compute a safe removal order of nodes except the given keeps.
--
-- 1st param: The acyclic dependency graph.
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
removeExceptSort :: Ord p => Graph p -> [p] -> RemoveExcept p
removeExceptSort depGraph keeps
  | null notFound = Remove removes
  | otherwise = NotFound notFound
  where
    notFound = filter (not . (`isVertex` depGraph)) keeps
    allKeeps = concatMap flatten (dfsFroms keeps depGraph)
    removes = reverseFinishSort (deleteVertices allKeeps depGraph)

-- | Answer type of removeOnly below.
data RemoveOnly p =
    RemoveOnly {oRemoves :: [p],       -- ^ safe removal order
                oNeeds :: [(p, [p])],  -- ^ unremovables with root causes (ancestors)
                oNotFound :: [p]       -- ^ nodes not found
               }
    deriving Show

-- | Compute a safe removal order of the requested nodes, insofar as they exist
-- and won't have unremoved ancestors.
--
-- Note that some requested nodes may be unremovable because they still have
-- unremoved ancestors.  They are excluded from the removal order.
--
-- The answer consists of: removal order of removables; unremovables and their
-- respective root causes (ancestors); nodes not found in the graph.
removeOnlySort :: Ord p => Graph p -> [p] -> RemoveOnly p
removeOnlySort depGraph ps = RemoveOnly{oRemoves, oNeeds, oNotFound}
  where
    oNotFound = filter (not . (`isVertex` depGraph)) ps
    candidates = reverseFinishSort (subgraph ps depGraph)
    ((needed, oRemoves), newRevDeps) =
        runState (partitionEithers <$> mapM classify candidates) (transpose depGraph)
    classify p = do
        removable <- gets (null . adjSet' p)
        if removable then do
            modify' (deleteVertex p)
            pure (Right p)
          else pure (Left p)
    -- Tricky: During classify, we can know whether a package is needed, but
    -- we cannot be sure who will be root ancestors.  Example:
    -- X -> Y -> Z.
    -- User asks to remove X and Z, but not Y.
    -- Subgraph of {X, Z} has no edge, topological sort can put them in
    -- any order, it can be [Z, X].
    -- During classify Z, we see X<-Y<-Z, but it is premature to conclude that X
    -- is root ancestor.  (X will be removed later, the correct root will be Y,
    -- this is knowable only after we have the final newRevDeps.)
    oNeeds = [(p, bottomsFrom newRevDeps p) | p <- needed]

-- | Childless descendents of a node.
bottomsFrom :: Ord p => Graph p -> p -> [p]
bottomsFrom g p = concatMap perTree (subForest (dfsFrom p g))
  where
    perTree Node{rootLabel, subForest}
      | not (null subForest) = concatMap perTree subForest
      | null (adjSet' rootLabel g) = [rootLabel]
      | otherwise = []


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
