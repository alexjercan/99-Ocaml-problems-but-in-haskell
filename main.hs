module Main where
import Data.List (group)

-- Problem 01
myLast :: [a] -> Maybe a
myLast [] = Nothing
myLast [x] = Just x
myLast (_:xs) = myLast xs

-- Problem 02
myTwoLast :: [a] -> Maybe (a, a)
myTwoLast [] = Nothing
myTwoLast [_] = Nothing
myTwoLast [x, y] = Just (x, y)
myTwoLast (_:xs) = myTwoLast xs

-- Problem 03
myAt :: Int -> [a] -> Maybe a
myAt _ [] = Nothing
myAt 0 (x:xs) = Just x
myAt n (x:xs) = myAt (n - 1) xs

-- Problem 04
myLength :: [a] -> Int
myLength = foldr (const (+1)) 0

-- Problem 05
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

-- Problem 06
myPalindrome :: Eq a => [a] -> Bool
myPalindrome xs = xs == myReverse xs

-- Problem 07
myFlatten :: [[a]] -> [a]
myFlatten = concat

myFlatten' :: [[a]] -> [a]
myFlatten' xs = go [] xs
    where go acc [] = acc
          go acc (x:xs) = go (acc++x) xs

-- Problem 08
myCompress :: Eq a => [a] -> [a]
myCompress = map head . group

myCompress' :: Eq a => [a] -> [a]
myCompress' [] = []
myCompress' [x] = [x]
myCompress' (x:y:xs)
    | x == y = myCompress' (y:xs)
    | otherwise = x : myCompress' (y:xs)

-- Problem 09
myPack :: Eq a => [a] -> [[a]]
myPack = group

myPack' :: Eq a => [a] -> [[a]]
myPack' = myReverse . go [] []
    where go _ _ [] = []
          go curr acc [x] = (x : curr) : acc
          go curr acc (x:xs@(y:_))
            | x == y = go (x : curr) acc xs
            | otherwise = go [] ((x : curr) : acc) xs

-- Problem 10
myEncode :: Eq a => [a] -> [(Int, a)]
myEncode xs = zip (map length gs) (map head gs)
    where gs = group xs

myEncode' :: Eq a => [a] -> [(Int, a)]
myEncode' xs = zipWith f gs gs
    where gs = group xs
          f = flip ((,) . length) . head

main :: IO ()
main = undefined

