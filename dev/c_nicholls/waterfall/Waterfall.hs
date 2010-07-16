{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE  FlexibleInstances #-}
module Waterfall(waterfalls,Edge(..),mkTree,Tree,Mergeable(union,unions),getRegion,getEdges) where
--import Data.Tree (Tree(..),Forest)
--import qualified Data.Foldable as F
import Data.List(sort)
import qualified Data.Set  as Set

data Tree a = Node a [Edge a]
{- Invariant: The list of edges are sorted by weight (ascending) -}

mkTree :: a -> [Edge a] -> Tree a
mkTree r es = Node r (sort  es)

data  Edge a = Edge Int (Tree a)

waterfalls :: Mergeable a => Tree a -> [Tree a]
waterfalls = takeWhile (hasChildren). iterate waterfall


waterfall :: Mergeable a =>   Tree a -> Tree a
waterfall  (Node r []) = Node r []
waterfall  (Node r ((Edge w child):cs))
  | hasChildren child && minVal child <  w
               = mergeNodes (waterfall child) (wndm (Node r cs))
  | otherwise  = mergeNodes (wndm child )     (wndm (Node r cs))


wndm :: Mergeable a =>   Tree a -> Tree a
wndm  (Node r []) = Node r []
wndm  (Node r ((Edge w child):cs))
  | hasChildren child && minVal child <  w
               = isertAsChild (Edge w (waterfall child)) (wndm (Node r cs))
  | otherwise  = mergeNodes (wndm child )      (wndm (Node r cs))



mergeNodes :: Mergeable a => Tree a -> Tree a -> Tree a
mergeNodes (Node r1 cs1) (Node r2 cs2) = Node (union r1 r2) (merge cs1 cs2)

isertAsChild :: Mergeable a => Edge a -> Tree a ->  Tree a
isertAsChild e (Node r es)  =  Node r (e:es)

-- Auxiliary functions

merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
  | x <= y = x : merge xs (y:ys)
  | otherwise = y : merge  (x:xs) ys

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

minVal :: Tree a  -> Int
minVal = minimum.map getWeight.getEdges

class Mergeable a where
  union :: a -> a -> a
  unions :: [a] -> a

instance Eq (Edge a) where
  (Edge w1 _) == (Edge w2 _) = w1 == w2

instance Ord (Edge a) where
  compare (Edge w1 _) (Edge w2 _) = compare w1 w2

instance Mergeable [Char] where
  union a b  = a ++ b
  unions = foldr union ""


instance Ord a => Mergeable (Set.Set (Int,a)) where
  union = Set.union
  unions = Set.unions

