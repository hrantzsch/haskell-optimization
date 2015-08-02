module Algorithms where

import           BinaryString
import           Control.Monad
import qualified System.Random as R
import qualified Data.Sequence as S

flipBit :: Int -> Int
flipBit = (1-)

mutate :: BinaryString -> IO BinaryString
-- currently RLS, change it later to flip more bits
mutate bs = do
  gen <- R.newStdGen
  let (index, _) = R.randomR (0,S.length bs-1) gen
  return $ S.adjust flipBit index bs

maxFitness :: (BinaryString -> Int) -> BinaryString -> BinaryString -> BinaryString
maxFitness f a b
  | f b > f a = b
  | otherwise = a

onePlusOneEA :: (BinaryString -> Int) -> Int -> IO BinaryString
onePlusOneEA f n = do
  gen <- R.newStdGen
  let best = S.fromList $ take n $ R.randomRs (0,1) gen
  print best
  offspring <- mutate best
  return $ maxFitness f best offspring
  -- replicateM_ 10 $ do
  --   print best
  --   offspring <- mutate best
  --   let best = offspring
  --   print best
  --   return ()
  -- return best
