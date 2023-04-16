{-# language NamedFieldPuns #-}

-- | My homebrew minimalist graph library.
module MonoGraph (
    -- * Graph and smart constructors
    Graph(..),
    fromAdjs,
    completeAdjs,
    fromEdges,
    -- * Getters
    --
    -- | Asking about vertices, edges, and adjacencies
    isVertex,
    isEdge,
    toAdjLists,
    vertexSet,
    vertices,
    edges,
    adjSet,
    adjSet',
    adjList,
    adjList',
    -- * Subtractions
    --
    -- | Deleting some edges or vertices.
    deleteEdge,
    deleteVertex,
    deleteVertices,
    deleteVertexSet,
    subgraph,
    subsetgraph,
    -- * Transpose
    transpose,
    -- * DFS and friends
    --
    -- | DFS with various scopes (single source, multiple sources, whole graph),
    -- and DFS-based algorithms: topological sort, strongly-connected
    -- components.
    --
    -- Also re-exports Data.Tree.flatten for flattening DFS trees to lists—many
    -- applications just want a list of reachable vertices in any order.
    dfsFrom,
    dfsFroms,
    dfs,
    flatten,
    reverseFinish,
    reverseFinishSort,
    scc,
    topologicalSort,
    ) where

import           Control.Monad.State.Lazy
import           Data.Foldable (fold)
import           Data.List (foldl')
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Tree (Tree(..), flatten)

-- | The graph type.  v is the vertex type.
newtype Graph v  = Graph (Map v (Set v))
    deriving Show

-- Helper function: The natural conversion as suggested by the type.
containerize :: Ord v => [(v, [v])] -> Map v (Set v)
containerize xs = Map.fromList (map (fmap Set.fromList) xs)

-- | Make a graph from adjacency lists.
--
-- A “truncation” is performed: For example (note how 25 is handled),
--
-- > fromAdjs [(0, [1, 25]), (1, [])] = fromAdjs [(0, [1]), (1, [])]
--
-- In general, vertices that don't appear on the “from” side are discarded.
fromAdjs :: Ord v => [(v, [v])] -> Graph v
fromAdjs xs = Graph final
  where
    draft = containerize xs
    domain = Map.keysSet draft
    final = fmap (Set.intersection domain) draft
-- Carefully walking xs just once.

-- | Make a graph from adjacency lists.
--
-- A “completion” is performed: For example (note how 25 is handled),
--
-- > completeAdjs [(0, [1, 25]), (1, [])] = completeAdjs [(0, [1, 25]), (1, []), (25, [])]
--
-- In general, vertices that appear on the “to” side are included on the “from” side.
completeAdjs :: Ord v => [(v, [v])] -> Graph v
completeAdjs xs = Graph (primary `Map.union` extras)
  where
    primary = containerize xs
    extras = Map.fromSet (const Set.empty) (fold primary)
    -- fold primary uses the fact that <> for Set is union.
-- Carefully walking xs just once.

-- | Make a graph from edges.
fromEdges :: Ord v => [(v, v)] -> Graph v
fromEdges es = Graph d
  where
    op m (u,v) = Map.insertWith (\_ x -> x) v Set.empty
                   (Map.insertWith Set.union u (Set.singleton v) m)
    d = foldl' op Map.empty es
-- This is contrived to walk es only once.

-- | List of adjacency lists.
toAdjLists :: Graph v -> [(v, [v])]
toAdjLists (Graph d) = [(u, Set.toList vs) | (u, vs) <- Map.toList d]

-- | Set of vertices.
vertexSet :: Graph v -> Set v
vertexSet (Graph d) = Map.keysSet d

-- | List of vertices.
vertices :: Graph v -> [v]
vertices (Graph d) = Map.keys d

-- | Edge list of a graph.
edges :: Graph v -> [(v, v)]
edges (Graph d) = [(u, v) | (u, vs) <- Map.toList d, v <- Set.toList vs]

-- | @Just@ the adjacency set of a vertex, or @Nothing@ if none.
adjSet :: Ord v => v -> Graph v -> Maybe (Set v)
adjSet v (Graph d) = Map.lookup v d

-- | The adjacency set of a vertex.
-- It is an error if the vertex is not in the graph.
adjSet' :: Ord v => v -> Graph v -> Set v
adjSet' v (Graph d) = d Map.! v

-- | @Just@ The adjacency list of a vertex, or @Nothing@ if none.
adjList :: Ord v => v -> Graph v -> Maybe [v]
adjList v g = fmap Set.toList (adjSet v g)

-- | The adjacency list of a vertex.
-- It is an error if the vertex is not in the graph.
adjList' :: Ord v => v -> Graph v -> [v]
adjList' v g = Set.toList (adjSet' v g)

-- | Check whether the given vertex is in the graph.
isVertex :: Ord v => v -> Graph v -> Bool
isVertex v (Graph d) = v `Map.member` d

infix 4 `isVertex`

-- | Check whether edge (u,v) is in the graph.
--
-- This works even when u or v is not a vertex in the graph; the answer is then
-- False.
isEdge :: Ord v => (v, v) -> Graph v -> Bool
isEdge (u, v) (Graph d) = case Map.lookup u d of
  Nothing -> False
  Just vs -> v `Set.member` vs

infix 4 `isEdge`

-- | Transpose of the graph.
transpose :: Ord v => Graph v -> Graph v
transpose g@(Graph d) = Graph d'
  where
    es = edges g
    d' = foldl' op (fmap (const Set.empty) d) es
    op m (v,u) = Map.insertWith Set.union u (Set.singleton v) m
-- Remark: edges is helpful, but fromEdges is inadequate!  Do not think that
-- this will work: fromEdges. map swap . edges
-- What's missing is vertices that don't participate in any edge.

-- | Delete an edge (u,v).  No harm done if u, v, or (u,v) was not in the graph.
deleteEdge :: Ord v => (v, v) -> Graph v -> Graph v
deleteEdge (u, v) (Graph d) = Graph d'
  where
    d' = Map.adjust (Set.delete v) u d

-- | Delete a vertex.  No harm done if the vertex was not in the graph.
deleteVertex :: Ord v => v -> Graph v -> Graph v
deleteVertex v (Graph d) = Graph d'
  where
    d' = fmap (Set.delete v) (Map.delete v d)

-- | Delete a whole lot of vertices.  No harm done if some were not in the graph.
deleteVertices :: Ord v => [v] -> Graph v -> Graph v
deleteVertices vs g = deleteVertexSet (Set.fromList vs) g

-- | Delete a set of vertices.  No harm done if some were not in the graph.
deleteVertexSet :: Ord v => Set v -> Graph v -> Graph v
deleteVertexSet s (Graph d) = Graph d'
  where
    d' = fmap (Set.\\ s) (d `Map.withoutKeys` s)

-- | Subgraph induced by the specified vertices, i.e., keep only those vertices
-- but as many edges as possible.  No harm done if some were not in the graph.
subgraph :: Ord v => [v] -> Graph v -> Graph v
subgraph vs g = subsetgraph (Set.fromList vs) g

-- | Subgraph induced by the specified vertex set, i.e., keep only those vertices
-- but as many edges as possible.  No harm done if some were not in the graph.
subsetgraph :: Ord v => Set v -> Graph v -> Graph v
subsetgraph s (Graph d) = Graph d'
  where
    d' = fmap (Set.intersection s) (d `Map.restrictKeys` s)

-- | DFS starting from a specified vertex.
--
-- It is an error if the vertex is not in the graph.
dfsFrom :: Ord v => v -> Graph v -> Tree v
dfsFrom v0 (Graph dict) = case evalState (dfsVisits [v0]) dict of
  [] -> error "dfsFrom used on a non-existent vertex"
  t:_ -> t

-- The state is the adjacency list representation of the unvisited graph,
-- e.g., it could be the Map-Set version of
-- [(2, [0, 3]), (3, [0, 2])]
-- meaning that only 2 and 3 are still unvisited.
-- In the to-lists, 0 is a relic, it's already visited, we don't bother updating.

dfsVisits :: Ord v => [v] -> State (Map v (Set v)) [Tree v]
dfsVisits [] = pure []
dfsVisits (v : vs) = do
    m <- gets (Map.lookup v)
    case m of
      Nothing -> dfsVisits vs
      Just wset -> do
          modify' (Map.delete v)
          -- Start v
          t <- Node v <$> dfsVisits (Set.toList wset)
          -- Finish v
          ts <- dfsVisits vs
          pure (t : ts)

-- | DFS starting from the specified vertices in the specified order.
--
-- But note that the output list can have fewer trees because some specified
-- vertices become visited by the time we get to them, so no corresponding
-- trees.
--
-- Non-existent vertices do not cause errors, but they are considered “visited”
-- in the sense above, i.e., no corresponding trees.
dfsFroms :: Ord v => [v] -> Graph v -> [Tree v]
dfsFroms vs (Graph dict) = evalState (dfsVisits vs) dict

-- | DFS over the whole graph, starting/restarting from implementation-chosen
-- vertices.
dfs :: Ord v => Graph v -> [Tree v]
dfs (Graph dict) = evalState go dict
  where
    go = do
        m <- gets Map.lookupMin
        case m of
          Nothing -> pure []
          Just (v0, _) -> do
              t <- head <$> dfsVisits [v0]
              ts <- go
              pure (t : ts)

-- | If the input is a DFS forest from e.g. 'dfs' above, the output is the
-- vertices in the reverse-finish order.  (In such a DFS forest, a parent
-- finishes later than a child, and siblings are in finish order.)
--
-- This order is pivotal in e.g. topological sort and identifying
-- strongly-connected components.
reverseFinish :: [Tree v] -> [v]
reverseFinish ts = concatMap perTree (reverse ts)
  where
    perTree t@Node{rootLabel, subForest} = rootLabel : reverseFinish subForest

-- | reverseFinishSort = reverseFinish . dfs
--
-- If the graph has no cycle, this is topological sort.
reverseFinishSort :: Ord v => Graph v -> [v]
reverseFinishSort = reverseFinish . dfs

-- | Strongly-connected components.  Each output tree spans one component.  The
-- list is in topological sort order: If component C1 has some edge to component
-- C2, then C1 is listed before C2.
--
-- Indeed, scc g = dfsFroms (reverseFinishSort g) (transpose g)
scc :: Ord v => Graph v -> [Tree v]
scc g = dfsFroms (reverseFinishSort g) (transpose g)

-- | Topological sort (@Right@) or detect a cycle (@Left@, reports a DFS
-- tree of an SCC).
--
-- Currently this uses the overkill algorithm of doing 'scc' first, then check
-- that every component is a singleton. :)
--
-- If you already know that there is no cycle, 'reverseFinishSort' is faster.
topologicalSort :: Ord v => Graph v -> Either (Tree v) [v]
topologicalSort g = traverse f (scc g)
  where
    f t@Node{rootLabel, subForest}
      | null subForest = Right rootLabel
      | otherwise = Left t
