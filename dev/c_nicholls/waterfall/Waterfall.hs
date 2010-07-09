{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE  FlexibleInstances #-}
module Waterfall(waterfall, Tree(..),Edge(..),Mergeable(union,unions),getRegion,getEdges) where

--import Data.Tree (Tree(..),Forest)
--import qualified Data.Foldable as F


data Tree a = Node a [Edge a]
data Edge a = Edge Int (Tree a) | Elide Int (Tree a)

class Mergeable a where
  union :: a -> a -> a
  unions :: [a] -> a

waterfall ::Mergeable a => Edge a ->  Edge a
waterfall e  =
  let Edge w t = mark e in Edge w (elide t)



mark ::Mergeable a =>  Edge a -> Edge a
mark (Edge w (Node a [])) = Elide w (Node a [])
mark (Edge w (Node a cs))
  | w <= getWeight e = Elide w (Node a (map mark cs))
  | otherwise       = Edge w (Node a (toElide e : es))
  where
    (e , es) = findMin (\t1 t2 -> compare (getWeight t1) (getWeight t2)) (map mark cs)

elide ::Mergeable a =>  Tree a -> Tree a
elide (Node a [])   = Node a []
elide (Node a (es)) = elideEdges es a

elideEdges ::Mergeable a =>  [Edge a] ->  a -> Tree a
elideEdges [] a = Node a []
elideEdges (Edge w t : es) a = let (Node b cs ) =(elideEdges es a) in Node b (Edge w (elide t) : cs)
elideEdges (Elide _ t : es)  a =  let (Node b cs ) = elideEdges es a in let Node c ds = (elide t) in Node (union b c  ) (cs ++ds)



toElide (Edge  w t ) = Elide w t
toElide e = e
{-
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

iter :: (a,Int) ->Tree (a,Int) ->  Tree (a,Int)
iter e (Node x cs) = f e (fold x cs)

fold :: (a,Int) -> Forest (a,Int) -> Tree (a,Int)
fold x [] = Node x []
fold x (c:cs) =
  let Node y ds = fold x cs in
  let Node z es = f y c in Node z (ds ++ es)

-}




-- to call on every node,

-----------------------------------------

findMin :: (a -> a -> Ordering) ->  [a] -> (a,[a])
findMin cmp  (z:zs) = foldr (\x (e,es)-> case (cmp x e) of
  LT -> (x,e:es)
  _ -> (e,x:es)) (z,[]) zs
findMin _ [] = error "The impossible happened"

-- The weight of an edge
getWeight :: Edge  a  -> Int
getWeight (Edge  w _) = w
getWeight (Elide  w _) = w

-- The region in a node
getRegion :: Tree a -> a
getRegion (Node r _) = r

-- The list of edges originating from a node
getEdges :: Tree a-> [Edge a]
getEdges (Node _ es) = es

instance Mergeable [Char] where
  union a b  = a ++ b
  unions = foldr union ""



