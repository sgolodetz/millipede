{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE  FlexibleInstances #-}
module Waterfall(waterfall, Tree(.. ),Edge(),Mergeable(union,unions),getRegion,getEdges) where
import Debug.Trace
--import Data.Tree (Tree(..),Forest)
--import qualified Data.Foldable as F


data Tree a = Node a [(Tree a,Int)]

type Edge a = (Tree a,Int)

class Mergeable a where
  union :: a -> a -> a
  unions :: [a] -> a



getRegion :: Tree a -> a
getRegion (Node  r _ ) = r

getEdges :: Tree a  -> [Edge a]
getEdges (Node  _ es) =  es
{-
f :: Tree a -> Int -> Tree a ->
f (Node a  []) = Node a []
f (Node a ((w,Node b bs):as)) = f (Node b ((w,Node a as):bs))
-}

{-
There are four types of child
 1) The child we want to merge with that wants to merge with us
 2) The child we want to merge with that doesn't want to merge with us
 3) A child we don't want to merge with that wants to merge with us
 4) A child we don't want to merge with that doesn't want to merge with us

 1) Recurse (making sure it won't merge down) and then merge region and children.
 2) Recurse and then merge with it.
 3) Recurse (making sure it won't merge down) and then merge region and children.
 4) Simply recurse, this edge is staying put.

1&3 are identical
2&4 begin in the same way, then recombine differently.

So we want:

recurseWithNoDownMerging :: Tree a -> Tree a

Then we extract the smallest child c.
  recurseWithNoDownMerging on this node with the rest of the children
  recurse on c if it doesn't want to merge up.
  recurseWithNodownMerging if c does want to merge up.
-}

waterfall :: Mergeable a => Tree a -> Tree a
waterfall (Node r []) = Node r []
waterfall (Node r cs)
  | hasChildren child && minVal child <  w = Node r' ((waterfall child,w) :cs')
  | otherwise        = Node (union r' childr) (childcs ++ cs')
  where
    Node childr childcs = recurseWithNoDownMerging child
    Node r' cs'         = recurseWithNoDownMerging (Node r rest)
    ((child,w),rest) = findMin cs

recurseWithNoDownMerging :: Mergeable a => Tree a -> Tree a
recurseWithNoDownMerging (Node r []) =  (Node r [])
recurseWithNoDownMerging (Node r ((c,w):cs))
  | hasChildren c && minVal c < w = Node r' ((waterfall c,w):cs') -- case 4
  | otherwise    = Node (union r' r2) (cs'++ds)   -- case 3
  where
    Node r' cs' = recurseWithNoDownMerging (Node r cs)
    Node r2 ds = waterfall c

minVal :: Tree a  -> Int
minVal = minimum.map snd.getEdges

findMin :: [(a,Int)] -> ((a,Int),[(a,Int)])
findMin [] = error "empty findmin"
findMin [a] = (a,[])
findMin ((a,w):xs)
  | w < m = ((a,w),(b,m):ys)
  | otherwise = ((b,m),(a,w):ys)
  where
    ((b,m),ys) = findMin xs


hasChildren :: Tree a -> Bool
hasChildren = not.childless

childless :: Tree a ->  Bool
childless (Node _ cs) = null cs
-- Look at the children and pull them up if the
{-
f :: Mergeable a => Tree a -> Tree a
f (Node m r  []) = error "no children"
f (Node m r cs) =
  where
    childrenThatWantToMergeUp = [n | (w,n) <- cs, (childless n || getMin n >= w) ]
    childWithWhichWeWishToMerge = findMin cs

-}



{-


When entering f, we assume that the current node does not want to
merge up (it's parent may want to merge, however).

-}
--f (Node min1 _ cs) | min1 /= minimum (map fst cs) = error ( "min invarient broken " ++ show (min1 : map fst cs))

{-
f (Node min1 r1 ((w,Node min2 r2 cs2):cs1))
  | w == min1  = (Node (min min1 min2') (union r1'' r2') (cs1'' ++ cs2'))
  | w < min2  = (Node (min min1' min2') (union r1' r2') (cs1' ++ cs2'))
  | otherwise  = Node  (min w min1')             r1'            ((w,Node min2' r2'  cs2') :cs1')
  where
    Node min1' r1' cs1' = f (Node (minimum (map fst cs1)) r1 cs1)
    Node _ r1'' cs1'' = f (Node 10000 r1 cs1)
    Node min2' r2' cs2' = f (Node min2 r2 cs2)
-}




-- The weight of an edge


-- The list of edges originating from a node


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
