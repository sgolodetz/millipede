module SkewHeap 
  (Heap
  ,mkHeap
  ,merge
  ,insert
  ,deleteMin
  ,heapToList
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

mkHeap :: Ord a => [a] -> Heap a
mkHeap as = foldl (\y x -> merge (single x) y) Leaf as

insert :: Ord a => Heap a -> a -> Heap a
insert h a = merge h (single a)

single :: Ord a => a -> Heap a
single a = Node a Leaf Leaf


deleteMin:: Ord a => Heap a -> (a,Heap a)
deleteMin (Node a l r) = (a,merge l r)

heapToList :: Ord a=> Heap a -> [a]
heapToList Leaf = []
heapToList a = x:heapToList b
  where (x,b) = deleteMin a



