module Util where
import Data.Char
import Data.List
import System.IO

-- Get the coordinate char as an index
charToIndex :: Char -> Int
charToIndex c = (ord c) - 97

intToIndex :: Int -> Int
intToIndex x = 8 - x


innerleave:: [a] -> [a] -> [a]
innerleave x [] = []
innerleave x [y] = [y]
innerleave x (y:ys) = [y] ++ x ++ innerleave x ys

interleave :: a -> [a] -> [a]
interleave x [] = []
interleave x [y] = [x] ++ [y] ++ [x]
interleave x (y:ys) = x : y : interleave x ys

rmdups :: (Ord a) => [a] -> [a]
rmdups = map head . group . sort

isBetween :: Int -> Int -> Int -> Bool
isBetween x n val = val >= x && val <= n

moveFits file rank x = isBetween 0 7 (file + x) && isBetween 0 7 (rank + x)

replace :: (Int, Int) -> a -> [[a]] -> [[a]]
replace (i,j) val container = chop 8 (x ++ [val] ++ xs)
    where (x,b:xs) = splitAt (j * 8 + i) (concat container)


chop :: Int -> [a] -> [[a]]
chop n [] = []
chop n xs = take n xs : chop n (drop n xs)

-- splitEvery 2 [x1,x2,x3,..,xn] -> [[x1,x2],[x3,x4],...]
splitEvery n = takeWhile (not . null) . unfoldr (Just . splitAt n)

-- Only defined for 0..9 
itsInt :: Char -> Int
itsInt x = (ord x) - 48
itsChr :: Int -> Char 
itsChr x =  chr(x+48)
