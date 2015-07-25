module Main where

import BinaryString
import qualified Problems as P

main :: IO ()
main = do
  let result = P.oneMax [I,I,O,O,I]
  -- putStrLn $ concatMap show (toInts result)
  print result
