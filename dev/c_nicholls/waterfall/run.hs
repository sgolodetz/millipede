import Process(bounds,output,getAdjacencyList,arrayToNode,freeze'')
import PGM(arraysToFile,
  arrayToFile,
  pgmsFromFile)
import Waterfall(waterfall,getNode)
import Data.Time.Clock(getCurrentTime,diffUTCTime,NominalDiffTime)

import System(getArgs)

main = do 
  {t0 <- getCurrentTime
  ;putStrLn "Reading file"
  ;args <- getArgs
  ;Right (ar:as) <- 	pgmsFromFile (head args)
  ;t1 <- timeSince t0
  ;putStrLn "Generating MST"
  ;es <- getAdjacencyList  ar
  ;t2 <- timeSince t1
  ;putStrLn "Making Tree"	

  ;edge <- arrayToNode es (-1,-1) (0,(0,(0,0)))
  ;t3 <- timeSince t2
  ;putStrLn "Doing Waterfall"
  ;let bs = bounds ar
  ;let trees = (take 5$!drop 1 $! iterate (waterfall$!)$!getNode edge)
  ;t4 <- timeSince t3
  ;putStrLn "Converting output"
  ;ars <- mapM (output bs) trees
  ;t5 <- timeSince t4
  ;save 1 (ars)
  ;t4 <- timeSince t5
  ;putStrLn "Done"
  ;timeSince t0
  }


save n [a] =do {arrayToFile ("output"++show n++".pgm") a}
save n (a:as) = do 
  {putStrLn ("Saving file "++show n)
  ;arrayToFile ("output"++show n++".pgm") a
  ;save (n+1) as
  }


timeSince t = do
  {t' <- getCurrentTime
  ;(putStrLn.show. (\x ->  (fromInteger (round (x*10000)) :: NominalDiffTime)/10000)) (diffUTCTime t' t)
  ;return t'
  }
