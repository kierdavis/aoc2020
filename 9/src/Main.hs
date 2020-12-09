module Main where

import Data.List ( subsequences, tails )

preambleLength :: Int
preambleLength = 25

main :: IO ()
main = do
  input <- getContents
  let numbers = map read (lines input) :: [Int]
  let x = firstInvalid numbers
  putStrLn $ show x
  putStrLn $ show (weakness x numbers)

firstInvalid :: [Int] -> Int
firstInvalid = last . head . filter (not . isValidWindow) . windows (preambleLength+1)

windows :: Int -> [a] -> [[a]]
windows n = filter ((==n) . length) . map (take n) . tails

isValidWindow :: [Int] -> Bool
isValidWindow w = elem target sums
  where
    (addends, target) = (init w, last w)
    sums = map sum (combinations 2 addends)

combinations :: Int -> [a] -> [[a]]
combinations 1 xs = map pure xs
combinations n (x:xs) = map (x:) (combinations (n-1) xs) ++ combinations n xs
combinations _ [] = []

weakness :: Int -> [Int] -> Int
weakness target (x:xs) = case summingPrefix target (x:xs) of
  Just prefix -> foldr1 min prefix + foldr1 max prefix
  Nothing     -> weakness target xs

summingPrefix :: Int -> [Int] -> Maybe [Int]
summingPrefix target (x:xs)
  | target < 0  = Nothing
  | target == 0 = Just []
  | otherwise   = (x:) <$> summingPrefix (target-x) xs
