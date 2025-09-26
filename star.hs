{-# LANGUAGE LambdaCase #-}

import Data.List (nub, sortOn)
import Data.Tuple (swap)
import Nn
import Random
import System.Posix (epochTime)
import System.Process (rawSystem)

ix :: Int -> [x] -> x
ix ix xs = xs !! (ix - 1)

type TrainingData = [([Double], [Double])]

newSeed :: IO Integer
newSeed = fmap (random . read . show) epochTime

teacher :: IO TrainingData
teacherS :: TrainingData -> [Integer] -> IO TrainingData
teacher = fmap (iterate random) newSeed >>= teacherS []
teacherS store seeds =
  do
  let points = randomFrom box <$> take 17 seeds
      player@(pa, pb) = randomFrom box $ ix 18 seeds
      bourne@(ba, bb) = randomFrom box $ ix 19 seeds
      sense = flip mod 4 $ ix 20 seeds
      layerA = drawRectangle 'x' 7 9 points
      layerB = drawRectangle 'o' 7 9 [player]
      layerC = drawRectangle '_' 7 9 [bourne]
      map = (write layerC . write layerB) layerA
  displayMap layerA
  wait 1
  displayMap layerB
  wait 1
  displayMap layerC
  wait 1
  displayMap map
  putStrLn "how would you move?"
  move <-
    retry (
      do {
        putStr " ";
        (\case
          'h' -> Just 4
          'j' -> Just 1
          'k' -> Just 3
          'l' -> Just 2
          '2' -> Just 1
          '6' -> Just 2
          '8' -> Just 4
          '4' -> Just 8
          _ -> Nothing
          ) <$>
          getChar
        }
      )
  putStrLn "\ndenote which free moves there are by binary addition\n1: south  2: east  4: north  8: west"
  free <-
    retry (
      do {
        putStr " ";
        (\input ->
          if elem input (words "1 2 3 4 5 6 7 8 9 10 11 12 13 14 15")
            then Just (read input)
           else Nothing
          ) <$>
          getLine
        }
      )
  if 4 == move && 1 == free
    then print store
   else
   teacherS
   (
     ((,)
       [free, toEnum pa, toEnum pb, toEnum ba, toEnum bb, fromIntegral sense]
       [move]
     ) :
     store
     )
   (drop 20 seeds)
  where wait t = const () <$> rawSystem "sleep" [show t]

wait :: Int -> IO ()
wait t = const () <$> rawSystem "sleep" [show t]

retry :: IO (Maybe a) -> IO a
retry action =
  action >>= \out -> case out of
    Just out -> pure out
    Nothing -> retry action

box :: [(Int, Int)]
box = [(a, b) | b <- [1 .. 9], a <- [1 .. 7]]

randomFrom :: [x] -> Integer -> x
randomFrom xs big =
  let biglength = toEnum (length xs) in
  xs !! fromEnum (mod big biglength)

drawRectangle :: Char -> Int -> Int -> [(Int, Int)] -> String
drawRectangle brush aa bb points = drawRectangleC (1, 1) brush aa bb ((nub . sortOn swap) points ++ repeat (0, 0))
drawRectangleC (ac, bc) brush aa bb (point : points)
  | ac - 1 == aa = '\n' : continue (1, bc + 1) (point : points)
  | bc - 1 == bb = []
  | (ac, bc) == point = brush : continue (ac + 1, bc) points
  | otherwise = '.' : continue (ac + 1, bc) (point : points)
  where continue cursor points = drawRectangleC cursor brush aa bb points

write :: String -> String -> String
write [] [] = []
write ('\n' : hyper) ('\n' : hypo) = '\n' : write hyper hypo
write ('.' : hyper) (a : hypo) = a : write hyper hypo
write (b : hyper) (_ : hypo) = b : write hyper hypo

displayMap :: String -> IO ()
displayMap map = do {rawSystem "clear" []; putStr map}

--

d001 = [([9.0,7.0,6.0,4.0,9.0,2.0],[4.0]),([3.0,6.0,5.0,6.0,3.0,2.0],[2.0]),([9.0,7.0,6.0,5.0,7.0,2.0],[1.0]),([11.0,2.0,4.0,2.0,2.0,2.0],[4.0]),([5.0,1.0,2.0,5.0,6.0,2.0],[1.0])]
