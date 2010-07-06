module Bucket
  (Bucket
  ,getMin
  ,fromList
  ,inserts
  )
where

newtype Bucket v = Bucket [[v]] deriving Show

mkBucket :: Bucket a
mkBucket  = Bucket (repeat [])



insert :: Bucket (Int,v) -> (Int , v) -> Bucket (Int,v)
insert (Bucket ls) (n,v) = Bucket (insertList ls n (n,v))


insertList :: [[v]] -> Int -> v -> [[v]]
insertList (ls:lss) 0 v = (v:ls):lss
insertList (ls:lss) n v = ls:insertList lss (n-1) v

getMin :: Bucket (Int,v) ->  ((Int,v),Bucket (Int,v))
getMin (Bucket lss) = let (v,lss') = getMinList lss in (v, Bucket lss')

getMinList :: [[v]] -> (v,[[v]])
getMinList ((l:ls):lss) = (l,ls:lss)
getMinList ([]:lss) = let (l,lss') = getMinList lss in (l,[]:lss')

fromList :: [(Int,a)] -> Bucket (Int,a)
fromList [] = mkBucket
fromList ((x,e):xs) = insert (fromList xs)  (x,e)


inserts :: Bucket (Int, v) -> [(Int,v)] -> Bucket (Int,v)
inserts b = foldl insert b
