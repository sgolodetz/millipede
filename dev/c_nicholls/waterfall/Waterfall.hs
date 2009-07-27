module Waterfall(waterfall,Tree(Node),Edge,Mergable(union),ListMergable) where

import List (sortBy)
data Tree a = Node a [Edge a] 
type Edge a = (Tree a,Int)

-- f applied to an edge returns the edge after merging. The boolean value indicates 
-- whether or not the edge contained a regional minimum amongst its children

class Mergable a where
  union :: a -> a -> a

waterfall :: Mergable a => Tree a -> Tree a
waterfall (Node r []) = Node r []
waterfall (Node r es) = fst.fst$f (Node r es,m)
  where
    m = maximum [i| (t,i) <- es]  +1
{- f:
To combine all the correct edges:
From a given node, with weight w leaing to it,
we first apply f to each of its chlidren to get the 
correctly merged sub-trees along with a boolean value 
representing whether or not that child has been merged 
with a regional minima below it. 

If the node is a leaf, then we're done. It is not
merged with anything below it. 

If none of the children where merged with a RM then we 
are safe to merge with all the children since this won't 
bring two RM together. If w is graeter than any of the weights 
leading to the children then our node has been merged with a RM.

Assume that some of the children merged with a RM. We still want 
to merge with every child that did not merge with a RM since doing
so will not join any RM but we may also want to merge with one of
the children that did since it may have greatest claim over the 
current node. A child will have this claim if the weight leading 
to it is less than the weights of all other edges leading to the
curretn node. 
The node we're at merged with a RM if it
-merged with one of its children that had merged or if
-any of its children that did not merge with a RM formed a RM themselves. 
 This relies on the same condition as before: that w is not smaller than
 the weight of any edge leading away from the node. 
-}

data MergeFlag = LessEq | Greater
  deriving (Eq,Ord)

f :: Mergable a => Edge a -> (Edge a,MergeFlag)
f n@(Node r [],w) = (n,LessEq)
f (Node r es,w) = 
  let es' = map f es  in
  let es'' = sortBy cmpEdge es' in
  let ((e,w'),b) = head es'' in 
  let es''' = ((e,w'),LessEq):tail es'' in 
  let cond =  (b==Greater && (w'<=w)) in 
    case cond of 
    True   ->  (join r es''' w, Greater)
    False  ->  (join r es''  w, flag (w'>=w)) 
    

join ::  Mergable a=> a -> [(Edge a,MergeFlag)] -> Int -> Edge a
join r [] w = (Node r [],w)
join r ((e,b):es) w 
 | b == LessEq  = (Node r''  es'' , w')
 | otherwise = (Node r' (e:es'), w')
  where
    r'' = (union r' (getRegion e))
    es'' = (es' ++ getEdges e)
    (Node r' es', w') = join r es w

flag True = LessEq
flag False = Greater

assert False x = x
assert True _ = error "Error" 

-- Auxillary functions 

cmpEdge :: Mergable a =>(Edge a,MergeFlag) -> (Edge a,MergeFlag) -> Ordering
cmpEdge ((x,w1),b1) ((y,w2),b2) 
  | w1 == w2 = compare b2 b1
  | otherwise = compare w1 w2

type Region = [Char]


class ListMergable a where
  listMerge :: [a] -> [a] -> [a]

instance ListMergable a where
  listMerge a b = a

instance ListMergable a => Mergable [a] where
  union = listMerge


getRegion :: Edge a -> a
getRegion (Node r es,n) = r

getEdges :: Edge a-> [Edge a]
getEdges (Node r es,n) = es

weightOf :: Edge a -> Int 
weightOf (Node r es,w) = w

---- Stuff for testing ----------

main = ((putStr.show). waterfall) tree1

tree1 = Node ("a") -- should merge [abci] [jk] [mldefgh]
    [(Node ("b") [(Node ("c") [(Node "n" [],1)],6)] ,5)
    ,(Node "m" [(Node "l" [(Node ("d")
        [(Node ("e") [(Node ("f") [],2)],1)
        ,(Node ("g") [(Node ("h") [],3)],1)
        ],4)],5)],5)
    ,(Node ("i")
        [(Node ("j") [(Node ("k") [] ,2)],3)],2)
    ]
tree2 = Node ("a") -- [abcil] [defgh] [jk] or same as t1
    [(Node ("b") [(Node ("c") [],6)] ,2)
    ,(Node "l" [(Node ("d")
        [(Node ("e") [(Node ("f") [],2)],1)
        ,(Node ("g") [(Node ("h") [],3)],1)
        ],4)],4)
    ,(Node ("i")
        [(Node ("j") [(Node ("k") [] ,2)],3)],2)
        
    ]
edge = (Node "l" [(Node ("d")
        [(Node ("e") [(Node ("f") [],2)],1)
        ,(Node ("g") [(Node ("h") [],3)],1)
        ],4)],5)

tree3= Node "a" [(Node "b" [(Node "c" [(Node "d" [], 0)], 0)] ,1)] --[abcd]

tree4 = Node "a" --[abcd] [efg] [hi] | [cd] [abhi] [efg]
  [(Node "b" 
    [(Node "c" [(Node "d" [],1)],2)
    ,(Node "e" [(Node "f" [(Node "g" [],1)],2)],3)
    ,(Node "h" [(Node "i" [],1)],2)
    ],4)
  ]
  
  
tree5 = --[ag] [bcdf]
  Node "a" 
   [(Node "b" 
    [(Node "c" [(Node "d" [],7)],6)
    ,(Node "e" [(Node "f" [],4)],3)
    ],5)
   ,(Node "g" [],2)]

tree6 = 
  Node "a" 
   [(Node "b" 
    [(Node "c" [(Node "d" [],5)],5)
    ,(Node "e" [(Node "f" [],5)],5)
    ,(Node "g" [(Node "h" [],1)],6)
    ,(Node "i" [(Node "j" [],1)],6)
    ],5)
   ]


instance Show a =>  Show (Tree a) where
  show t = show' 1 (t,0) ++"\n"
  
show' :: Show a => Int -> (Tree a,Int) -> [Char]
show' n ((Node r []),k) = show k++"->"++ show r 
show' n ((Node r ts),k) = show k++ " -> " ++ show r   ++ (concat$map (( ("\n"++(f  n "    "))++).(show' (n+1))) ts)
  where 
    f 0 t = ""
    f (n+1) t = t ++ f n t 
    
    
