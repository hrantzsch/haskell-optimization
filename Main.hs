module Main where

import qualified Algorithms   as A
import qualified Problems     as P

main :: IO ()
main = do
  -- let result = P.oneMax [1,0,0,1,1]
  -- putStrLn $ concatMap show (toInts result)
  result <- A.onePlusOneEA P.oneMax 10
  print result
