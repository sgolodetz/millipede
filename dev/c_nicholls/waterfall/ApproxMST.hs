{-# LANGUAGE FlexibleContexts, BangPatterns #-}
module Process(
  bounds,
  output,
  buildMST,
  arrayToNode,freeze'',
  Adjacency,Point,Voxel(Voxel)
    ) where

import Data.List(foldl')
import SkewHeap (Heap,fromList,merge,insert,getMin,toList  )
import Data.Array.IO
import Data.Array.Unboxed       (IArray, Ix, UArray, amap, bounds, elems, listArray, (!)  )
import List(sort,sortBy)
import Data.Word                (Word8, Word16)
import Waterfall(waterfall,Node,mkNode,Edge,mkEdge,Mergeable(union),getRegion,getEdges,getNode,getWeight)
import PGM
  ( pgmToArray,
    pgmsToArrays,
    pgmToArrayWithComments, pgmsToArraysWithComments,
    arrayToPgmWithComment,
    pgmsFromFile, pgmsFromHandle,
    arrayToPgm, arrayToFile, arrayToHandle, arraysToHandle,
    arraysToFile
  )

type Adjacency  = (Int,Voxel,Voxel)
type Region = [Point]
type Point = (Int,Int)
data Voxel = Voxel Int Point
  deriving (Ord,Eq,Show)


---- IO Array Functions ----            
fillIn :: Node (Heap Voxel) -> IOUArray Point Int -> IO (IOUArray Point Int)
fillIn node ar = do
  {let rs' = toList (getRegion node)
  ;let x = avg rs'
  ;mapM (\(Voxel v p) -> writeArray ar p x) rs'
  ;ar' <- foldR (fillIn.getNode) ar (getEdges node)
  ;return $!  ar
  }

arrayToNode :: IOArray Point [(Int, Voxel)] -> Point -> (Int, Voxel) -> IO (Edge (Heap Voxel))
arrayToNode arr miss (n,(Voxel v p))  = do
  {
  ;ls <- (readArray arr p)
  ;let ls' = remove miss ls
  ;let ls'' = remove p ls'
  ;es <- mapM  (arrayToNode arr p) ls''
  ;return $! mkEdge (mkNode(fromList [(Voxel v p)]) es) n
  }

remove :: Point -> [(Int,Voxel)] -> [(Int,Voxel)]
remove p [] = []
remove p ((w,(Voxel v a)):ps) 
  | a ==p = remove p ps
  | otherwise = ((w,(Voxel v a)):remove p ps)


-- Builds the approximate-MST by looking at each node and picking the
-- smallest out of the two edges leading up and right.
-- Produces bad results mut is interesting to compare
-- for memory/time usage.


--map and fold: for each node(except (0,0) ), pick the smallest edge 
--  and add it to the MST 
buildMST :: (IArray UArray Int) =>  UArray Point Int -> IO (IOArray Point [(Int,Voxel)])
buildMST arr =  foldl' ((>>=)) brr list
  where
  ((a,b),(c,d)) = bounds arr
  brr  = newArray ((a,b),(c,d)) [] :: IO (IOArray Point [(Int,Voxel)]  )
  list = map (addNodeToMST arr) ( [(x,y) | x<-[0..c], y<-[0..d]])
  -- list :: [IOArray Point [(Int,Voxel)] -> IO (IOArray Point [(Int,Voxel)]) ]
  
  
-- point1 is the node we're concidering, point2 is then the edge we pick.
addNodeToMST :: UArray Point Int -> Point -> IOArray Point [(Int,Voxel)] -> IO (IOArray Point [(Int,Voxel)])
addNodeToMST !arr !point1 !brr  = do
  {let point2 = pickEdge arr point1
  ;let v1 = arr!point1
  ;let v2 = arr!point2
  ;l1 <- readArray brr point1
  ;l2 <- readArray brr point2
  ;writeArray brr point1 ((abs(v1-v2),(Voxel v2 point2)):l1)
  ;writeArray brr point2 ((abs(v1-v2),(Voxel v1 point1)):l2)
  ;return  $! brr
  }

pickEdge :: UArray Point Int -> Point -> Point
pickEdge !arr !(n,m)
 | n==0 && m ==0  = (0,0) --error "picking from the corner"
 | m==0 = (n-1,0)
 | n==0 = (0,m-1)
 | arr!(n-1,m) < arr!(n,m-1) = (n-1,m)
 | otherwise = (n,m-1)


---  Main  -------------  
output :: (Point, Point) -> Node (Heap Voxel) -> IO (UArray Point Word16)
output bounds tree = do
  {arr <- newArray bounds 0 :: IO (IOUArray Point Int)
  ;arrr <- fillIn tree arr
  ;arrrr <- freeze' arrr
  ;let arrrrr = amap (fromIntegral :: Int -> Word16) arrrr
  ;return $!  arrrrr
  }


--- Other Functions ---------
instance Ord a => Mergeable (Heap a) where
  union = merge

freeze' :: ( MArray a Int IO, IArray UArray Int) => a Point Int -> IO (UArray Point Int)
freeze' = freeze

freeze'' :: ( MArray a b IO, IArray UArray b) => a Point b -> IO (UArray Point b)
freeze'' = freeze

    
foldR            :: (Monad m) => (a -> b -> m b) -> b -> [a] -> m b
foldR f a []     = return $!  a
foldR f a (x:xs) = foldR f a xs >>= \y -> f x y

avg :: [Voxel] -> Int
avg [] = 200
avg [x] = 200
avg ((Voxel x (a,b)):xs) = mod ((a+b)*13 + 17*(avg xs) ) 255

pointOf (Voxel v p) = p






