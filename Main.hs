module Main where

import qualified Algorithms as A
import qualified Problems   as P

main :: IO ()
main = do
  let problem = P.royalRoads5
      stringLength = 1000
      maxIterations = 10000
  result <- A.optimize problem stringLength maxIterations
  print result
  putStrLn $ "Fitness: " ++ show (problem result)
