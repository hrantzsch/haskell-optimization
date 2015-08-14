module Main where

import qualified Algorithms as A
import qualified Problems   as P

main :: IO ()
main = do
  -- let result = P.oneMax [1,0,0,1,1]
  -- putStrLn $ concatMap show (toInts result)
  result <- A.onePlusOneEA (P.royalRoads 10) 100
  print result
