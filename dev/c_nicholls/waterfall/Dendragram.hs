{-# LANGUAGE  FlexibleInstances #-}
module Dendragram(dendragram) where

import  Waterfall(mkNode,Node(Node),Edge(Edge),mkEdge,Mergeable(union),getRegion,getEdges,getNode,getWeight) 




dendragram :: Mergeable a => Int ->Node a ->  [Node a]
dendragram x n  = n':dendragram (x+1) n'
  where n' = elide x n

applyToNode :: (Node a-> Node a) -> Edge a-> Edge a
applyToNode f (Edge a n) = Edge a (f n)


elide ::Mergeable a =>   Int -> Node a ->Node a
elide n (Node r [])  = Node r []
elide n (Node r es)  = Node region children
  where
    region = foldr (union.getRegion.getNode) r ms
    children = us ++ (foldr ((++).getEdges.getNode) [] ms)
    (ms,us) = splitWith ((<n).getWeight)   (map (applyToNode (elide n)) es)



-- Auxillary functions 

splitWith :: (a -> Bool) -> [a] -> ([a],[a])
splitWith f [] = ([],[])
splitWith f (x:xs)
  | f x = (x:as,bs)
  | otherwise = (as,x:bs)
  where (as,bs) = splitWith f xs
  
