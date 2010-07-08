{-# OPTIONS_GHC -Wall#-}
import Process(bounds,output,getAdjacencyList,arrayToNode,save,Voxel(Voxel) )

import Waterfall(waterfall)
import Data.Time.Clock(getCurrentTime,diffUTCTime,NominalDiffTime,UTCTime)
import PGM( pgmsFromFile)
import System.Environment(getArgs)

main :: IO()
main = do
  t0 <- getCurrentTime
  putStrLn "Reading file"
  args <- getArgs
  Right (ar:_) <-   pgmsFromFile (head args)

  t1 <- timeSince t0
  putStrLn "Generating MST"
  es <- getAdjacencyList  ar

  t2 <- timeSince t1
  putStrLn "Making Tree"
  edge <- arrayToNode es (-1,-1) (1000000000,(Voxel 0 (0,0)))

  t3 <- timeSince t2
  putStrLn "Doing Waterfall"
  let bs = bounds ar
  let trees = (take 6 $ tail  $ iterate (waterfall)$ edge)

  t4 <- (timeSince t3)
  putStrLn "Converting output"
  ars <- mapM (output bs) trees

  t5 <- timeSince t4
  save path 1 (ars)
  _ <- timeSince t5
  putStrLn "Done"
  _ <- timeSince t0
  return ()

path :: String
path="./output/output"




timeSince ::UTCTime -> IO UTCTime
timeSince t = do
  {t' <- getCurrentTime
  ;(putStrLn.show. (\x ->  (fromInteger (round (x*10000)) :: NominalDiffTime)/10000)) (diffUTCTime t' t)
  ;return t'
  }
