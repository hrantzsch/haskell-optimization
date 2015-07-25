module Problems where

import qualified Data.List as L
import Data.Maybe (fromMaybe)
import BinaryString

oneMax :: BinaryString -> Int
oneMax = sum . toInts

leadingOnes :: BinaryString -> Int
leadingOnes bs =
  fromMaybe (length asInt) (L.elemIndex 0 asInt)
  where asInt = toInts bs

binVal :: BinaryString -> Int
binVal bs = sum $ map f b
  where f (x,y) = y * 2 ^ x
        asInts = toInts bs
        b = zip [0..length asInts] $ reverse asInts
