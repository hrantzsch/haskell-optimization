module Problems where

import           BinaryString
import           Data.Maybe    (fromMaybe)
import qualified Data.Sequence as S

oneMax :: BinaryString -> Int
oneMax = sum

leadingOnes :: BinaryString -> Int
leadingOnes bs =
  fromMaybe (length bs) (S.elemIndexL 0 bs)

binVal :: BinaryString -> Int
binVal bs = sum $ fmap f b
  where f (x,y) = y * 2 ^ x
        b = S.zip (S.fromList [0..S.length bs]) $ S.reverse bs
