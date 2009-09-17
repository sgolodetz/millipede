{-# LANGUAGE  FlexibleInstances #-}
module Waterfall(waterfall,mkNode,Node(Node),Edge(Edge),mkEdge,Mergeable(union),getRegion,getEdges,getNode,getWeight) where

import List (sortBy)
data Node a = Node !a ![Edge a] 
data Edge a = Edge Int (Node a) 

-- f applied to an edge returns the edge after merging. The boolean value indicates 
-- whether or not the edge contained a regional minimum amongst its children

class Mergeable a where
  union :: a -> a -> a

waterfall :: Mergeable a => Node a -> Node a
waterfall (Node r []) = Node r []
waterfall (Node r es) = (getNode.fst.mergeChildren) (Edge  (maximum [i| Edge i t <- es]) (Node r es))
  
-- f :: Mergeable a => Edge a -> (Edge a,Bool)
-- f n@(Edge w (Node r [])) = (n,False)
-- f (Edge w (Node r es) ) = 
--   let ((e,b):es') = sortBy cmpEdge (map f es) in
--   let w' = getWeight e in 
--     case (b==True && (w'<=w)) of 
--     True   ->  (join r ((e,False): es') w, True)
--     False  ->  (join r ((e,b): es')  w, flag (w'>=w)) 
  
mergeChildren :: Mergeable a => Edge a -> (Edge a,Bool)
mergeChildren n@(Edge w (Node r [])) = (n,False)
mergeChildren (Edge w (Node r es) ) 
  | (b && w'<=w) = (join r ((e,False): es') w, True)
  | otherwise    = (join r ((e,b): es')  w, (w'<w)) 
  where 
    ((e,b):es') = sortBy cmpEdge (map mergeChildren es)
    w' = getWeight e 

{--
recurse: es''
merge the minimum child
merge the non
--}

-- join ::  Mergeable a=> a -> [(Edge a,Bool)] -> Int -> Edge a
-- join r [] w = Edge w (Node r [])
-- join r ((Edge v n,b):es) w 
--  | b   =        Edge w' (Node r' ((Edge v n ):es')) 
--  | otherwise  = Edge w' (Node r''  es'') 
--   where
--     r'' = (union r' (getRegion n))
--     es'' = (es' ++ getEdges n)
--     Edge w' (Node r' es') = join r es w


join ::  Mergeable a=> a -> [(Edge a,Bool)] -> Int -> Edge a
join r [] w = Edge w (Node r [])
join r ((Edge v n,b):es) w 
 | b   =        Edge w' (Node r' ((Edge v n ):es')) 
 | otherwise  = Edge w' (Node r''  es'') 
  where
    r'' = (union r' (getRegion n))
    es'' = (es' ++ getEdges n)
    Edge w' (Node r' es') = join r es w

extractEdgeRegions :: [Edge a] -> ([a],[Edge a])
extractEdgeRegions [] = ([],[])
extractEdgeRegions ((Edge v n):es) =
  ( (getRegion n):as, (getChildren n)++bs )
  where (as,bs) = split xs



split :: [(a,Bool)] -> ([a],[a])
split [] = ([],[])
split ((x,b):xs)
  | b =( x:as,bs)
  | otherwise = (as,x:bs)
  where (as,bs) = split xs



-- Auxillary functions 


cmpEdge :: Mergeable a =>(Edge a,Bool) -> (Edge a,Bool) -> Ordering
cmpEdge ((Edge w1 x),b1) ((Edge w2 y),b2) 
  | w1 == w2 = compare b2 b1
  | otherwise = compare w1 w2

type Region = [Char]

getNode :: Edge a -> Node a
getNode (Edge w n) = n

getWeight (Edge w n ) = w

getRegion :: Node a -> a
getRegion (Node r es) = r

getEdges :: Node a-> [Edge a]
getEdges (Node r es) = es


mkNode :: Mergeable a =>a -> [Edge a]->  Node a
mkNode a es = Node a es

mkEdge :: Mergeable a => Node a -> Int -> Edge a
mkEdge n v = Edge v n


instance Mergeable [Char] where
  union a b  = a ++ b
---- Stuff for testing ----------

  {- f:
To combine all the correct edges:
From a given Node, with weight w leaing to it,
we first apply f to each of its chlidren to get the 
correctly merged sub-Nodes along with a boolean value 
representing whether or not that child has been merged 
with a regional minima below it. 

If the Node is a leaf, then we're done. It is not
merged with anything below it. 

If none of the children where merged with a RM then we 
are safe to merge with all the children since this won't 
bring two RM together. If w is graeter than any of the weights 
leading to the children then our Node has been merged with a RM.

Assume that some of the children merged with a RM. We still want 
to merge with every child that did not merge with a RM since doing
so will not join any RM but we may also want to merge with one of
the children that did since it may have greatest claim over the 
current Node. A child will have this claim if the weight leading 
to it is less than the weights of all other edges leading to the
curretn Node. 
The Node we're at merged with a RM if it
-merged with one of its children that had merged or if
-any of its children that did not merge with a RM formed a RM themselves. 
 This relies on the same condition as before: that w is not smaller than
 the weight of any edge leading away from the Node. 
-}  
    
