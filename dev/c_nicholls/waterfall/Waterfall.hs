{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE  FlexibleInstances #-}
module Waterfall(waterfall, mkNode,Tree(Node),Edge,Mergeable(union,unions),getRegion,getEdges,getWeight,size) where

import Data.Tree (Tree(..),Forest)



-- mergeChildren applied to an edge returns the edge after merging;
-- the boolean value indicates whether or not the edge contained
-- a regional minimum amongst its children.

type Edge a = Tree (a,Int)

class Mergeable a where
  union :: a -> a -> a
  unions :: [a] -> a

-- waterfall creates a (hypothetical) edge into the root of the MST
-- and then proceeds to call the main mergeChildren function.


waterfall :: Mergeable a => Edge a -> Edge a
waterfall = fst.mergeChildren

-- f was the initial mergeChildren function (now known as mergeChildren)
--
-- f :: Mergeable a => Edge a -> (Edge a,Bool)
-- f n@(Edge w (Node r [])) = (n,False)
-- f (Edge w (Node r es) ) =
--   let ((e,b):es') = sortBy cmpEdge (map f es) in
--   let w' = getWeight e in
--     case (b==True && (w'<=w)) of
--     True   ->  (join r ((e,False): es') w, True)
--     False  ->  (join r ((e,b): es')  w, flag (w'>=w))


-- mergeChildren walks the tree bottom-up and labels each edge
-- with True or False depending on whether the child has been joined
-- with a regional minimum below it (see also long explanation below)
--
-- it also calls the join function when regions need merging.
{-# SECIALIZE mergeChildren :: Edge (Set a) -> (Edge (Set a),Bool)#-}
{-# SECIALIZE mergeChildren :: Edge (Heap a) -> (Edge (Heap a),Bool)#-}
mergeChildren :: (Mergeable a )=> Edge a -> (Edge a,Bool)
mergeChildren n@(Node _ []) = (n,False)
mergeChildren (Node (r,w) es)
  | (b && w'<=w) = (join r ((e,False):es') w, True  )
  | otherwise    = (join r ((e,b)    :es') w, (w'<w))
  where
    ((e,b),es') =   findMin cmpEdge (map mergeChildren es)
    w' = getWeight e

{-
It would be nice to have the signature of findMin as

>findMin :: Ord a => [a] -> (a,[a])

rather than refer to an ordering function, but we cannot make
(Edge a, Bool) an instasnce of the Ord class without a being in the EQ
class, which it needn't be.
-}

findMin :: (a -> a -> Ordering) ->  [a] -> (a,[a])
findMin cmp  (z:zs) = foldr (\x (e,es)-> case (cmp x e) of
  LT -> (x,e:es)
  _ -> (e,x:es)) (z,[]) zs
findMin _ [] = error "The impossible happened"


-- Initial join function
-- (should recurse only once because the tree is built bottom-up)
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



-- join function, with fold instead of recursion
--
-- r is the region in the current node;
-- w is the weight of the edge into the current node;
-- ebs are the (bool-labelled) edges from the current node to its children;
--
-- select the False-labelled edges, find the nodes at the end of them and
-- merge r into the regions of those nodes; this gives a bigger region, newr;
--
-- take the children (edges) of those nodes, into es, and make them
-- children of the current node (whose region was r);
--
-- the newly-created node has an edge of (unchanged) weight w coming into it,
-- and is made of the new region, newr;
-- as edges it has all the old True-labelled edges, as well as the edges
-- of the children who have merged into r.



join ::  Mergeable a=> a -> [(Edge a,Bool)] -> Int -> Edge a
join r es w | r `seq` es `seq` w `seq` False = undefined
join r ebs w =
   (Node (newr,w) es)
   where
     (rs, es) = extractEdgeRegions ebs [] []
     newr = unions (r:rs)

-- Prepare for absorbing the regions in the children of a node
-- by putting the region of the current node(s) in a list (as),
-- and the regions of the children in a list (bs).

extractEdgeRegions :: [(Edge a,Bool)] ->[a] ->  [Edge a] -> ([a],[Edge a]) -- ##
extractEdgeRegions xs as rs  | xs `seq` as `seq` rs `seq` False = undefined
extractEdgeRegions [] as rs = (as,rs)
extractEdgeRegions ((e,True):es) as rs = extractEdgeRegions es as (e:rs)
extractEdgeRegions ((Node (r,_) cs,False):es) as rs = extractEdgeRegions es (r : as) ( cs ++ rs)



-- Auxiliary functions

-- Compare the weights of two edges.
-- Returns a comparison operator that can then be used in sort.

cmpEdge :: (Edge a,Bool) -> (Edge a,Bool) -> Ordering
cmpEdge ((Node (_,w1) _),b1) ((Node (_,w2) _),b2)
  | w1 == w2 = compare b2 b1
  | otherwise = compare w1 w2


-- The weight of an edge

getWeight :: Tree (a,Int) -> Int
getWeight (Node (_,w) _) = w

-- The region in a node

getRegion :: Tree a -> a
getRegion (Node r _) = r

-- The list of edges originating from a node

getEdges :: Tree a-> Forest a
getEdges (Node _ es) = es

-- The children nodes of a node

--getChildren :: Node a-> [Node a]
--getChildren n = map getNode (getEdges n)

-- Make a node from a region and some edges

mkNode :: a -> Forest a ->  Tree a
mkNode a es = Node a es

instance Mergeable [Char] where
  union a b  = a ++ b
  unions = foldr union ""
---- Stuff for testing ----------


  {- f (a.k.a. mergeChildren):

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





size :: Tree a -> Int
size (Node _ es) =  1+ sum (map size es)
