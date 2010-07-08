{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE  FlexibleInstances #-}
module Waterfall(waterfall, Tree(..),Edge,Mergeable(union,unions),getRegion,getEdges) where

import Data.Tree (Tree(..),Forest)
--import qualified Data.Foldable as F

type Edge a = Tree (a,Int)

class Mergeable a where
  union :: a -> a -> a
  unions :: [a] -> a

waterfall :: (Mergeable a,Show a) => Edge a -> Edge a
waterfall t =   g t (Node (e,232323) [])
  where
    e = unions[]

g :: Mergeable c => Tree (c,Int) -> Tree (c,Int) -> Tree (c,Int)
g (Node (r1,w1) es) (Node (r2,w0) xxx )
  | null es || w1 <= w2 =                 foldr g (Node (union r2 r1,w0) xxx) es
  | otherwise           =  Node (r2,w0)  (foldr g (Node (union r1 r3,w1) [] ) (cs ++ es') :xxx)
  where
  (Node (r3,w2) cs , es') = findMin (\t1 t2 -> compare (getWeight t1) (getWeight t2)) es

-----------------------------------------

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

instance Mergeable [Char] where
  union a b  = a ++ b
  unions = foldr union ""



