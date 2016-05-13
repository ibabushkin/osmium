module Lib where

import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.Query.DFS (dff)
import Data.List (find, partition)

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
