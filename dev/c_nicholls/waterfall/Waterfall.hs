{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE  FlexibleInstances #-}
module Waterfall(waterfalls,Edge(..),mkTree,Tree(Node),Mergeable(union,unions),getRegion,getEdges,getNode) where

import Data.List(sort)
import qualified Data.Set  as Set

data Tree a = Node a [Edge a]
{- Invariant: The list of edges are sorted by weight (ascending) -}

data  Edge a = Edge Int (Tree a)

waterfalls :: Mergeable a => Tree a -> [Tree a]
waterfalls = takeWhile hasChildren. iterate waterfall

waterfall :: Mergeable a => Tree a -> Tree a
waterfall (Node a []) = (Node a [])
waterfall (Node r (c:cs)) = fcase1and2 c (foldr fcase3and4 (Node r []) cs)


fcase3and4 :: Mergeable a => Edge a ->Tree a -> Tree a
fcase3and4 (Edge w t@(Node a cs))
  | hasChildren t && w > minVal t = addChild w (waterfall t)
  | otherwise = mergeNodes (foldr fcase3and4 (Node a []) cs)

fcase1and2 :: Mergeable a => Edge a ->Tree a -> Tree a
fcase1and2 (Edge w t@(Node a cs))
  | hasChildren t && w > minVal t = mergeNodes (waterfall t)
  | otherwise = mergeNodes (foldr fcase3and4 (Node a []) cs)



-- Auxiliary functions

addChild :: Int -> Tree a -> Tree a -> Tree a
addChild w t (Node a cs) = Node a (Edge w t : cs)

mkTree :: a -> [Edge a] -> Tree a
mkTree r es = Node r (sort  es)

mergeNodes :: Mergeable a => Tree a -> Tree a -> Tree a
mergeNodes (Node r1 cs1) (Node r2 cs2) = Node (union r1 r2) (merge cs1 cs2)

merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
  | x <= y = x :  xs ++ (y:ys)
  | otherwise = y :  (x:xs) ++ ys

hasChildren :: Tree a -> Bool
hasChildren = not.childless

childless :: Tree a ->  Bool
childless (Node _ cs) = null cs

getRegion :: Tree a -> a
getRegion (Node  r _ ) = r

getEdges :: Tree a  -> [Edge a]
getEdges (Node  _ es) =  es

getWeight :: Edge a -> Int
getWeight (Edge w _ ) = w

getNode :: Edge a -> Tree a
getNode (Edge _ ns) = ns

minVal :: Tree a  -> Int
minVal = minimum.map getWeight.getEdges

class Mergeable a where
  union :: a -> a -> a
  unions :: [a] -> a
  empty :: a
  empty = unions []

instance Eq (Edge a) where
  (Edge w1 _) == (Edge w2 _) = w1 == w2

instance Ord (Edge a) where
  compare (Edge w1 _) (Edge w2 _) = compare w1 w2

instance Mergeable [a] where
  union a b  = a ++ b
  unions = foldr union []

instance Ord a => Mergeable (Set.Set (Int,a)) where
  union = Set.union
  unions = Set.unions
