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
waterfall (Node r es) = (getNode.fst.markChildren) (Edge  (maximum [i| Edge i t <- es]) (Node r es))
  
-- f :: Mergeable a => Edge a -> (Edge a,Bool)
-- f n@(Edge w (Node r [])) = (n,False)
-- f (Edge w (Node r es) ) = 
--   let ((e,b):es') = sortBy cmpEdge (map f es) in
--   let w' = getWeight e in 
--     case (b==True && (w'<=w)) of 
--     True   ->  (join r ((e,False): es') w, True)
--     False  ->  (join r ((e,b): es')  w, flag (w'>=w)) 
  
markChildren :: Mergeable a => Edge a -> (Edge a,Bool)
markChildren n@(Edge w (Node r [])) = (n,False)
markChildren (Edge w (Node r es) ) 
  | (b && w'<=w) = (join r ((e,False): es') w, True)
  | otherwise    = (join r ((e,b): es')  w, (w'<w)) 
  where 
    ((e,b):es') = sortBy cmpEdge (map markChildren es)
    w' = getWeight e 


-- Initial join function
--
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
join r ebs w =
   (Edge w (Node newr newes))
   where 
   	 (rs, es) = extractEdgeRegions (filterFalse ebs) 
         newr = foldl union r rs  
         oldes = filterTrue ebs   
         newes = oldes ++ es       


-- Prepare for absorbing the regions in the children of a node
-- by putting the region of the current node(s) in a list (as),
-- and the regions of the children in a list (bs).

extractEdgeRegions :: [Edge a] -> ([a],[Edge a])
extractEdgeRegions [] = ([],[])
extractEdgeRegions ((Edge v n):es) =
  ( (getRegion n):as, (getEdges n)++bs )
  where (as,bs) = extractEdgeRegions es


-- Split a list with Boolean tags into two sublists,
-- one for the True tags ts, and one for the False tags fs.
-- Might be clearer if written as two filter functions.

split :: [(a,Bool)] -> ([a],[a])
split [] = ([],[])
split ((x,b):xs)
  | b =(x:ts , fs)
  | otherwise = (ts , x:fs)
  where (ts,fs) = split xs

filterTrue :: [(a,Bool)] -> [a]
filterTrue = map fst . filter ((==True).snd)

filterFalse :: [(a,Bool)] -> [a]
filterFalse = map fst . filter ((==False).snd)


-- Auxiliary functions 


-- Compare the weights of two edges.
-- Returns a comparison operator that can then be used in sort.

cmpEdge :: Mergeable a =>(Edge a,Bool) -> (Edge a,Bool) -> Ordering
cmpEdge ((Edge w1 x),b1) ((Edge w2 y),b2) 
  | w1 == w2 = compare b2 b1
  | otherwise = compare w1 w2

-- Labels for a region

type Region = [Char]

-- The node that en edge points to

getNode :: Edge a -> Node a
getNode (Edge w n) = n

-- The weight of an edge

getWeight :: Edge a -> Int
getWeight (Edge w n) = w

-- The region in a node

getRegion :: Node a -> a
getRegion (Node r es) = r

-- The list of edges originating from a node

getEdges :: Node a-> [Edge a]
getEdges (Node r es) = es

-- The children nodes of a node

getChildren :: Node a-> [Node a]
getChildren n = map getNode (getEdges n)

-- Make a node from a region and some edges

mkNode :: Mergeable a =>a -> [Edge a]->  Node a
mkNode a es = Node a es

-- Make an edge of given weight, which points to a node

mkEdge :: Mergeable a => Node a -> Int -> Edge a
mkEdge n v = Edge v n


instance Mergeable [Char] where
  union a b  = a ++ b
---- Stuff for testing ----------


  {- f (a.k.a. markChildren):

To combine all the correct edges: From a given Node, with weight w
leading to it, we first apply f to each of its chlidren to get the
correctly merged sub-Nodes along with a boolean value representing
whether or not that child has been merged with a regional minimum (RM)
below it.

If the Node is a leaf, then we're done. It is not merged with anything
below it.

If none of the children where merged with a RM then it is safe to
merge with all the children since this won't bring two RM together. If
w is greater than any of the weights leading to the children then our
Node has been merged with a RM.

Assume that some of the children merged with a RM. We still want to
merge with every child that did not merge with a RM since doing so
will not join any RM but we may also want to merge with one of the
children that did since it may have greatest claim over the current
Node. A child will have this claim if the weight leading to it is less
than the weights of all other edges leading to the curretn Node.

The Node we're at merged with a RM if it

 - merged with one of its children that had merged 

or if

 - any of its children that did not merge with a RM formed a RM
 themselves.  This relies on the same condition as before: that w is
 not smaller than the weight of any edge leading away from the Node.

-}  
    


