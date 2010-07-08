{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE  FlexibleInstances #-}
module Waterfall(waterfall, mkNode,Edge,Mergeable(union,unions),getRegion,getEdges) where

import Data.Tree (Tree(..),Forest)
--import qualified Data.Foldable as F
-- mergeChildren applied to an edge returns the edge after merging;
-- the boolean value indicates whether or not the edge contained
-- a regional minimum amongst its children.

type Edge a = Tree (a,Int)

class Mergeable a where
  union :: a -> a -> a
  unions :: [a] -> a

-- waterfall creates a (hypothetical) edge into the root of the MST
-- and then proceeds to call the main mergeChildren function.


waterfall :: (Mergeable a,Show a) => Edge a -> Edge a
waterfall t =   g t (e,232323) []
  where
    e = unions[]
{-
f :: (Mergeable b,Show b) => (b,Int) ->Tree (b,Int) ->  Tree (b,Int)
f (a,n) (Node (b,n2) es)  | trace (shows a.shows n .shows " ". shows b .shows n2.shows  " ".shows es $ "") False = undefined
f (a,n) (Node (b,n2) es)
  | n <= n2 = (Node (union a b,n) es)
  | otherwise = Node (a,n) [Node (union b c,n2) (cs++ es')]
  where
  (Node (c,n3) cs , es') = findMin (\t1 t2 -> compare (getWeight t1) (getWeight t2))es
-}

g :: Mergeable c => Tree (c,Int) -> (c,Int) -> Forest (c,Int) -> Tree (c,Int)
g (Node (r1,w1) es) (r2,w0) k
  | null es || w1 <= w2 = combine es  ((union r2 r1),w0) k
  | otherwise  =  Node (r2,w0)  ((combine (cs ++ es') ((union r1 r3),w1)[]) : k)
  where
  (Node (r3,w2) cs , es') = findMin (\t1 t2 -> compare (getWeight t1) (getWeight t2))es

combine :: Mergeable a => Forest (a,Int) -> (a, Int) -> Forest (a,Int) -> Tree (a,Int)
combine [] x k = Node x k
combine (t:ts) x k =
  let Node y bs =  g t x k in
  combine ts y bs



findMin :: (a -> a -> Ordering) ->  [a] -> (a,[a])
findMin cmp  (z:zs) = foldr (\x (e,es)-> case (cmp x e) of
  LT -> (x,e:es)
  _ -> (e,x:es)) (z,[]) zs
findMin _ [] = error "The impossible happened"


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



