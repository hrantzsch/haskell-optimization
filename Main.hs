module Main where

import qualified Algorithms as A
import qualified Problems   as P

main :: IO ()
main = do
  -- let result = P.oneMax [1,0,0,1,1]
  -- putStrLn $ concatMap show (toInts result)
  let problem = P.leadingOnes -- P.royalRoads 10
      stringLength = 1000
      maxIterations = 10000
  result <- A.optimize problem stringLength maxIterations
  print result
  putStrLn $ "Fitness: " ++ show (problem result)
