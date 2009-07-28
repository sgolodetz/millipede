import Process(size,bounds,output,getAdjacencyList,arrayToTree,freeze'')
import PGM(arraysToFile,
  arrayToFile,
  pgmsFromFile)
import Waterfall(waterfall)

import System(getArgs)
import Data.Graph.Inductive

main = do 
  {putStrLn "Reading file"
  ;args <- getArgs
  ;Right (ar:as) <- 	pgmsFromFile (head args)
  ;putStrLn "Generating MST"
  ;es <- getAdjacencyList  ar
  ;putStrLn "Making Tree"	

  ;(tree,x) <- arrayToTree es (-1,-1) (0,(0,(0,0)))
  ;putStrLn "Doing Waterfall"
  ;let bs = bounds ar
  ;let trees = (take 3$!drop 2 $! iterate (waterfall$!) tree)
  ;putStrLn "Saving output"
  ;ars <- mapM (output bs) trees
  ;save 1 (ars)
  ;putStrLn "Done"
  }


save n [a] =do {arrayToFile ("output"++show n++".pgm") a}
save n (a:as) = do 
  {putStrLn ("Saving file "++show n)
  ;arrayToFile ("output"++show n++".pgm") a
  ;save (n+1) as
  }



