module Waterfall(waterfall,mkNode,Node,Edge,mkEdge,Mergable(union),getRegion,getEdges,getNode,getWeight) where

import List (sortBy)
data Node a = Node !a ![Edge a] 
data Edge a = Edge (Node a) Int

-- f applied to an edge returns the edge after merging. The boolean value indicates 
-- whether or not the edge contained a regional minimum amongst its children

class Mergable a where
  union :: a -> a -> a

waterfall :: Mergable a => Node a -> Node a
waterfall (Node r []) = Node r []
waterfall (Node r es) = (getNode.fst.f) (Edge (Node r es) (maximum [i| Edge t i <- es]))
  
f :: Mergable a => Edge a -> (Edge a,Bool)
f n@(Edge (Node r [])w) = (n,False)
f (Edge (Node r es) w) = 
  let es'' = sortBy cmpEdge (map f es) in
  let (e,b) = head es'' in 
  let w' = getWeight e in 
  let es''' = (e,False):tail es'' in 
  let cond =  (b==True && (w'<=w)) in 
    case cond of 
    True   ->  (join r es''' w, True)
    False  ->  (join r es''  w, flag (w'>=w)) 
    

join ::  Mergable a=> a -> [(Edge a,Bool)] -> Int -> Edge a
join r [] w = Edge (Node r []) w
join r ((Edge n v,b):es) w 
 | b == False  = Edge (Node r''  es'')  w'
 | otherwise = Edge (Node r' ((Edge n v):es')) w'
  where
    r'' = (id (union r' (getRegion n)))
    es'' = (es' ++ getEdges n)
    Edge (Node r' es') w' = join r es w


-- Auxillary functions 




flag :: Bool -> Bool
flag True = False
flag False = True

cmpEdge :: Mergable a =>(Edge a,Bool) -> (Edge a,Bool) -> Ordering
cmpEdge ((Edge x w1),b1) ((Edge y w2),b2) 
  | w1 == w2 = compare b2 b1
  | otherwise = compare w1 w2

type Region = [Char]

getNode :: Edge a -> Node a
getNode (Edge n w) = n

getWeight (Edge n w) = w

getRegion :: Node a -> a
getRegion (Node r es) = r

getEdges :: Node a-> [Edge a]
getEdges (Node r es) = es


mkNode :: Mergable a =>a -> [Edge a]->  Node a
mkNode a es = Node a es

mkEdge :: Mergable a => Node a -> Int -> Edge a
mkEdge n v = Edge n v

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
    
