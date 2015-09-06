module Algorithms where

import           BinaryString
import           Control.Monad
import qualified Data.Sequence as S
import qualified System.Random as R

flipBit :: Int -> Int
flipBit = (1-)

flipBinaryString :: BinaryString -> BinaryString -> BinaryString
flipBinaryString = S.zipWith (-)

mutateRLS :: BinaryString -> IO BinaryString
-- currently RLS, change it later to flip more bits
mutateRLS bs = do
  gen <- R.newStdGen
  let (index, _) = R.randomR (0,S.length bs-1) gen
  return $ S.adjust flipBit index bs

mutateOnePlusOne :: BinaryString -> IO BinaryString
mutateOnePlusOne bs = do
  gen <- R.newStdGen
  let floatMask = R.randomRs (0,1) gen :: [Float]
  let prob = 1 / fromIntegral (S.length bs)
  let bitMask = map ((\b -> if b then 1 else 0) . (< prob)) $ take (S.length bs) floatMask
  putStrLn $ "1+1 EA: mutating " ++ show (sum bitMask) ++ " bit(s)"
  return $ flipBinaryString bs (S.fromList bitMask)

maxFitness :: (BinaryString -> Int) -> BinaryString -> BinaryString -> BinaryString
maxFitness f a b
  | f b > f a = b
  | otherwise = a

improveRec :: BinaryString -> Int -> (BinaryString -> Int) -> (BinaryString -> IO BinaryString) -> IO BinaryString
improveRec current iterations fitness mutate
  | iterations <= 0 = return current
  | otherwise       = do
                        -- print current
                        offspring <- mutate current
                        improveRec (maxFitness fitness offspring current) (iterations - 1) fitness mutate

optimize :: (BinaryString -> Int) -> Int -> Int -> IO BinaryString
optimize problem stringLength maxIterations = do
  gen <- R.newStdGen
  let initial = S.fromList $ take stringLength $ R.randomRs (0,1) gen
  improveRec initial maxIterations problem mutateOnePlusOne
