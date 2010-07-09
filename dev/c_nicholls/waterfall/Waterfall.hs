{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE  FlexibleInstances #-}
module Waterfall(waterfall, Tree(..),Edge(..),Mergeable(union,unions),getRegion,getEdges) where

--import Data.Tree (Tree(..),Forest)
--import qualified Data.Foldable as F


data Tree e a = Node a [e a]
data Edge a = Edge Int (Tree Edge a)
data Elides a = NoElide Int (Tree Elides a) | Elide Int (Tree Elides a)

class Mergeable a where
  union :: a -> a -> a
  unions :: [a] -> a

waterfall ::Mergeable a => Tree Edge a ->  Tree Edge a
waterfall t1 =  let NoElide 1000 t2 = mapMark (Edge 1000 t1) in (elide t2)

mapMark ::Mergeable a => Edge a -> Elides a
mapMark (Edge w (Node a cs)) = mark w (Node a (map mapMark cs))

--mark takes an edge to a node and marks the minimum node as one to be elided.
mark :: Mergeable a =>  Int -> Tree Elides a -> Elides a
mark  w (Node a []) = Elide w (Node a [])
mark  w (Node a cs)
  | w <=getWeight e = Elide w (Node a  cs)
  | otherwise       = NoElide w (Node a (toElide e : es))
  where
    (e , es) = findMin (\t1 t2 -> compare (getWeight t1) (getWeight t2)) cs


elide ::Mergeable a =>  Tree Elides a -> Tree Edge a
elide (Node a (es)) = elideEdges es a

elideEdges ::Mergeable a =>  [Elides a] ->  a -> Tree Edge a
elideEdges [] a = Node a []
elideEdges (NoElide w t : es)  a = let (Node b cs ) =(elideEdges es a) in Node b (Edge w (elide t) : cs)
elideEdges (Elide _ t : es) a =  let (Node b cs ) = elideEdges es a in let Node c ds = (elide t) in Node (union b c) (cs ++ds)


toElide :: Elides a -> Elides a
toElide (NoElide  w t ) = Elide w t
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
getWeight :: Elides   a  -> Int
getWeight (NoElide  w _) = w
getWeight (Elide  w _) = w

-- The region in a node
getRegion :: Tree e a -> a
getRegion (Node r _) = r

-- The list of edges originating from a node
getEdges :: Tree e a-> [e a]
getEdges (Node _ es) = es

instance Mergeable [Char] where
  union a b  = a ++ b
  unions = foldr union ""

{-
waterfall (Node ...)
let NoElide 1000 t2 = mapMark (Edge 1000 (Node ...)) in (elide t2)
...] mapMark (Edge 1000 (Node a cs)) [...
...]  mark 1000 (Node a (map mapMark cs)) [...
...]  NoElide 1000 (Node a (toElideMin $ map mapMark cs)) [...
elide (Node a (elideMin $ map mapMark cs))
elideEdges (toElideMin ( map mapMark cs))  a
-}
