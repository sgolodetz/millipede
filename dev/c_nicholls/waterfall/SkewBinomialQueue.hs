module SkewBinomialQueue  
  (Queue
  ,fromList
  ,merge
  ,insert
  ,getMin
  ,toList
  ) where 


type Rank = Int
   
data Tree a = Node  a  Rank  [Tree a]
  deriving Show
type Queue a= [Tree a]


root (Node x r 	cs) = x
rank (Node x r cs) = r


isEmpty [] = True
isEmpty _  = False


link t1@(Node x1 r1 c1) t2@(Node x2 r2 c2)
 | x1 <= x2  = Node x1 (r1+1) (t2:c1)
 | otherwise = Node x2 (r2+1) (t1:c2)


skewLink t0@(Node x0 r0 c0) t1@(Node x1 r1 c1) t2@(Node x2 r2 c2)
  | x1 <= x0 && x1 <= x2 = Node x1 (r1+1) (t0:t2:c1)
  | x2 <= x0 && x2 <= x1 = Node x2 (r2+1) (t0:t1:c2)
  | otherwise = Node x0 (r1+1) [t1,t2]


ins x [] = [x]
ins x (y:ts) 
  |rank x < rank y = x:y:ts
  |otherwise = ins (link x y) ts

uniqify [] = []
uniqify (t:ts) = ins t ts

meldUniq [] ts = ts
meldUniq ts [] = ts
meldUniq (t1:ts1) (t2:ts2)
  | rank t1 < rank t2 = t1:meldUniq ts1 (t2:ts2)
  | rank t2 < rank t1 = t2:meldUniq (t1:ts1) ts2
  | otherwise = ins (link t1 t2) (meldUniq ts1 ts2) 

insert :: Ord a =>  a -> Queue a -> Queue a	
insert x ts@(t1:t2:rest) 
  | rank t1 == rank t2 = (skewLink (Node x 0 []) t1 t2):rest
  | otherwise = (Node x 0 []) :ts
insert x ts = (Node x 0 [] ):ts

merge :: Ord a => 	Queue a -> Queue a -> Queue a
merge ts ts' = meldUniq (uniqify ts) (uniqify ts')

findMin [] = error "Min of Empty Queue"
findMin [t] = root t
findMin (t:ts) =
  let x = findMin ts in
  case (root t<x) of
    True -> root t
    False -> x



getMin' [t] = (t,[])
getMin' (t:ts) = 
  let (t',ts') = getMin' ts in
  case (root t < root t') of
    True -> (t,ts)
    False -> (t',t:ts')	


split :: Queue a -> [a] -> Queue a -> 	(Queue a, [a])
split ts xs [] = (ts,xs)
split ts xs (t:c) 
  | rank t == 0 = split ts ((root t):xs)  c
  | otherwise = split (t:ts) xs c

getMin :: Ord a =>  Queue a -> (a,Queue a)
getMin [] = error "Empty"
getMin ts = 
  let (Node x r c,ts1) = getMin' ts in
  let (ts2,xs') = split [] [] c in
  (x,foldr	 (insert)  (merge ts1 ts2 ) xs')

    
toList ::Ord a =>  Queue	 a -> [a]    
toList [] = []
toList ts = 
  let (t,ts') = getMin ts in 
  t:toList ts'
  
fromList [] = []
fromList (x:xs) = 
  let ts = fromList xs in
  insert x ts


  
