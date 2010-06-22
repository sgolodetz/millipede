module SkewHeap 
  (Heap
  ,fromList,toList
  ,merge
  ,insert
  ,getMin
  ) where
  
data Heap a = Node a (Heap a) (Heap a) | Leaf

findMin :: Heap a -> a
findMin (Node a l r) = a

merge :: Ord a =>  Heap a -> Heap a -> Heap a
merge a Leaf = a
merge Leaf b = b
merge a@(Node x l1 r1) b@(Node y l2 r2) 
  | x <= y    = (Node x (merge r1 b) l1 )
  | otherwise = (Node y (merge r2 a) l2)

fromList :: Ord a => [a] -> Heap a
fromList as = foldl (\y x -> merge (single x) y) Leaf as

insert :: Ord a => Heap a -> a -> Heap a
insert h a = id (merge h (single a))

single :: Ord a => a -> Heap a
single a = Node a Leaf Leaf


getMin:: Ord a => Heap a -> (a,Heap a)
getMin (Node a l r) = (a,merge l r)

toList :: Ord a=> Heap a -> [a]
toList Leaf = []
toList a = x:toList b
  where (x,b) = getMin a



