module MakeTrees where
import Process
  (Adjacency,Point)
import Waterfall(Tree(Node),Edge)



allAdjacencysFrom :: [Adjacency] -> Point -> [Adjacency]
allAdjacencysFrom es p = 
  [(n,a,b) | (n,a,b) <- es, a == p  ] 
  ++[(n,b,a) | (n,a,b) <- es, b == p ]


edgeListToTree :: [Adjacency]->Point ->Point -> Tree [Point]	
edgeListToTree es p miss = Node [p] (foldl f [] (allAdjacencysFrom es p))
  where
    f e (n,p,m)| m==miss   = e
               | otherwise =  (edgeListToTree es m p,n):e
