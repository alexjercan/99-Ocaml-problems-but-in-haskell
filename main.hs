module Main where
import Data.List (group, intercalate)

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

-- Problem 11
data RList a = ConsOne a (RList a)
             | ConsMany (Int, a) (RList a)
             | REmpty

instance Show a => Show (RList a) where
    show ys = "[" ++ intercalate "," (go ys) ++ "]"
        where go REmpty = []
              go (ConsOne x xs) = show x : go xs
              go (ConsMany x xs) = show x : go xs

myEncodeR :: Eq a => [a] -> RList a
myEncodeR = f . group
    where f [] = REmpty
          f (x:xs)
            | length x == 1 = ConsOne (head x) (f xs)
            | otherwise     = ConsMany (length x, head x) (f xs)

-- Problem 12
myDecodeR :: RList a -> [a]
myDecodeR REmpty = []
myDecodeR (ConsOne x xs) = x : myDecodeR xs
myDecodeR (ConsMany (l, x) xs) = replicate l x ++ myDecodeR xs

-- Problem 13
myEncodeRD :: Eq a => [a] -> RList a
myEncodeRD = go 1
    where go _ [] = REmpty
          go 1 [x] = ConsOne x REmpty
          go acc [x] = ConsMany (acc, x) REmpty
          go acc (x:xs@(y:_))
            | x /= y && acc == 1 = ConsOne x (go 1 xs)
            | x /= y             = ConsMany (acc, x) (go 1 xs)
            | otherwise = go (acc+1) xs

-- Problem 14
myDuplicate :: [a] -> [a]
myDuplicate [] = []
myDuplicate (x:xs) = x : x : myDuplicate xs

-- Problem 15
myReplicate :: [a] -> Int -> [a]
myReplicate [] _ = []
myReplicate (x:xs) n = replicate n x ++ myReplicate xs n

-- Problem 16
myDrop :: [a] -> Int -> [a]
myDrop = go 1
    where go _ [] _ = []
          go acc (x:xs) n
            | n == acc  = go 1 xs n
            | otherwise = x : go (acc+1) xs n

-- Problem 17
mySplit :: [a] -> Int -> ([a], [a])
mySplit xs n = splitAt n xs

main :: IO ()
main = undefined

