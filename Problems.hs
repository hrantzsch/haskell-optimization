module Problems where

import           BinaryString
import qualified Data.Foldable as F
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

royalRoads :: BinaryString -> Int -> Int
royalRoads bs k
  | S.null bs = 0
  | otherwise = allOnes firstK + royalRoads remainder k
                  where (firstK, remainder) = S.splitAt k bs
                        allOnes bs | all (==1) $ F.toList bs = 1
                                   | otherwise               = 0
