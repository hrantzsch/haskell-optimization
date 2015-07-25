module BinaryString where

data Binary = O | I
type BinaryString = [Binary]

toInt :: Binary -> Int
toInt O = 0
toInt I = 1

toInts :: BinaryString -> [Int]
toInts = map toInt
