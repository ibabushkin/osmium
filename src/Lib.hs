module Lib where

import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.Query.DFS (dffWith, topsort)
import Data.List (find, partition)
import Data.Maybe (listToMaybe)
import Data.Tree
import Data.Tuple (swap)

-- | a primitive control flow graph implementation
data CFG a b = CFG [LNode a] [LEdge b]
    deriving (Eq, Show)

-- | CFG's are graphs
instance Graph CFG where
    empty = CFG [] []
    isEmpty (CFG [] []) = True
    isEmpty _ = False
    match = nodeMatch
    mkGraph = CFG
    labNodes (CFG ns _) = ns

-- | match on a node context
nodeMatch :: Node -> CFG a b -> Decomp CFG a b
nodeMatch n gr@(CFG ns es)
    | Just ns' <- filterNodes n ns = (context, CFG ns' es')
    | otherwise = (Nothing, gr)
    where (inEdges, outEdges, es') = filterEdges n es
          context = (,,,) <$> pure inEdges <*> pure n <*>
              (snd <$> find ((==n) . fst) ns) <*> pure outEdges

-- | filter a node from a graph if present 
filterNodes :: Node -> [LNode a] -> Maybe [LNode a]
filterNodes n ns = case partition ((==n) . fst) ns of
                     ([n'], ns') -> Just ns'
                     _ -> Nothing

-- | filter a node's in- and outgoing edges from a graph
filterEdges :: Node -> [LEdge b] -> (Adj b, Adj b, [LEdge b])
filterEdges n = foldr helper ([], [], [])
    where helper e@(n', n'', b) (i, o, r)
              | n' == n && n'' == n = ((b, n''):i, (b, n''):o, r)
              | n' == n = (i, (b, n''):o, r)
              | n'' == n = ((b, n'):i, o, r)
              | otherwise = (i, o, e:r)

-- | CFG's can be dynamically changed (think of reduction)
instance DynGraph CFG where
    (ie, n, a, oe) & (CFG ns es) = CFG ((n, a):ns) $
        (map toInbound ie) ++ (map toInbound oe) ++ es
        where toInbound (b, n') = (n', n, b)
              toOutbound (b, n') = (n, n', b)

-- | construct a DFS tree representing node traversal and containing backedge
-- source nodes for each node visited
dfsTree :: Graph gr => Node -> gr a b -> Maybe (Tree (Node, [LEdge b]))
dfsTree n = listToMaybe . dffWith go [n]
    where go (inEdges, n, _, _) = (n, map makeLEdge inEdges)
          makeLEdge (label, from) = (from, n, label)

-- | postorder of the tree we got from a DFS traversal
-- Doesn't use folds because I'm too lazy to read how exactly containers
-- implements them, so it was easier to roll my own version
postorder :: Tree a -> [a]
postorder (Node a as) = (as >>= postorder) ++ [a]

-- | given the DFS tree of a graph, compute it's acyclic version
computeDAG :: DynGraph gr => gr a b -> Tree (Node, [LEdge b]) -> gr a b
computeDAG gr t = case foldr go [] t of
                    [] -> gr
                    es -> foldr (delEdge . toEdge) gr es
    where go (_, []) edges = edges
          go (_, es) edges = es ++ edges

-- | a path in a graph
type CFGPath = [LEdge Bool]

-- | a type representing a reaching condition of a node
data Condition
    = And [Condition]
    | Or [Condition]
    | TrueEdge Node Node
    | FalseEdge Node Node
    | Trivial
    | Impossible
    deriving (Show, Eq)

-- | convert a path in a CFG to a condition that can be analyzed and reduced
pathToCondition :: CFGPath -> Condition
pathToCondition = foldl go Trivial
    where go (And cond) (from, to, tag)
              | tag = And $ cond ++ [TrueEdge from to]
              | otherwise = And $ cond ++ [FalseEdge from to]
          go cond (from, to, tag)
              | tag = And [cond, TrueEdge from to]
              | otherwise = And [cond, FalseEdge from to]

simplify :: Condition -> Condition
simplify = undefined
