module Main where

import Control.Arrow ( Arrow((&&&)) )
import System.Random ( uniformR, mkStdGen )
import Data.List (nub, sortBy)
import Data.Function (on)

-- Problem 1. Tail of a list
-- >>> myLast ["a", "b", "c", "d"]
-- Just "d"
-- >>> myLast []
-- Nothing
myLast :: [a] -> Maybe a
myLast [] = Nothing
myLast [x] = Just x
myLast (_:xs) = myLast xs

-- Problem 2. Last two elements of a list
-- >>> myLastTwo ["a", "b", "c", "d"]
-- Just ("c", "d")
-- >>> myLastTwo ["a"]
-- Nothing
myLastTwo :: [a] -> Maybe (a, a)
myLastTwo [] = Nothing
myLastTwo [_] = Nothing
myLastTwo [x, y] = Just (x, y)
myLastTwo (_:xs) = myLastTwo xs

-- Problem 3. Kth element of a list
-- >>> myElementAt ["a", "b", "c", "d"] 2
-- >>> myElementAt ["a", "b", "c", "d"] 5
-- Just "c"
-- Nothing
myElementAt :: [a] -> Int -> Maybe a
myElementAt [] _ = Nothing
myElementAt (x:_) 0 = Just x
myElementAt (_:xs) n = myElementAt xs (n - 1)

-- Problem 4. Number of elements in a list
-- >>> myLength ["a", "b", "c", "d"]
-- 4
myLength :: [a] -> Int
myLength = myLengthTR 0
    where
        myLengthTR acc [] = acc
        myLengthTR acc (_:xs) = myLengthTR (acc + 1) xs

-- Problem 5. Reverse a list
-- >>> myReverse ["a", "b", "c", "d"]
-- ["d","c","b","a"]
myReverse :: [a] -> [a]
myReverse = myReverseTR []
    where
        myReverseTR acc [] = acc
        myReverseTR acc (x:xs) = myReverseTR (x:acc) xs

-- Problem 6. Find out whether a list is a palindrome
-- >>> myIsPalindrome [1,2,3]
-- False
-- >>> myIsPalindrome ["x","a","m","a","x"]
-- True
myIsPalindrome :: Eq a => [a] -> Bool
myIsPalindrome xs = xs == myReverse xs

-- Problem 7. Flatten a nested list structure
-- >>> myFlatten (One "a")
-- ["a"]
-- >>> myFlatten (Many [One "a", Many [One "b", One "c"], One "d"])
-- ["a","b","c","d"]
data MyNestedList a = One a | Many [MyNestedList a]
myFlatten :: MyNestedList a -> [a]
myFlatten (One x) = [x]
myFlatten (Many xs) = concatMap myFlatten xs

-- Problem 8. Eliminate consecutive duplicates of list elements
-- >>> myCompress ["a","a","a","a","b","c","c","a","a","d","e","e","e","e"]
-- ["a","b","c","a","d","e"]
myCompress :: Eq a => [a] -> [a]
myCompress = myCompressTR []
    where
        myCompressTR acc [] = acc
        myCompressTR acc (x:xs)
            | myLast acc == Just x = myCompressTR acc xs
            | otherwise = myCompressTR (acc ++ [x]) xs

-- Problem 9. Pack consecutive duplicates of list elements into sublists
-- >>> myPack ["a","a","a","a","b","c","c","a","a","d","e","e","e","e"]
-- [["a","a","a","a"],["b"],["c","c"],["a","a"],["d"],["e","e","e","e"]]
myPack :: Eq a => [a] -> [[a]]
myPack = myPackTR [] []
    where
        myPackTR res [] [] = res
        myPackTR res acc [] = res ++ [acc]
        myPackTR res [] (x:xs) = myPackTR res [x] xs
        myPackTR res acc (x:xs)
            | myLast acc == Just x = myPackTR res (acc ++ [x]) xs
            | otherwise = myPackTR (res ++ [acc]) [x] xs

-- Problem 10. Run length encoding of a list
-- >>> myEncode ["a","a","a","a","b","c","c","a","a","d","e","e","e","e"]
-- [(4,"a"),(1,"b"),(2,"c"),(2,"a"),(1,"d"),(4,"e")]
myEncode :: Eq a => [a] -> [(Int, a)]
myEncode = map (myLength &&& head) . myPack

-- Problem 11. Modified run length encoding
-- >>> myModifiedEncode ["a","a","a","a","b","c","c","a","a","d","e","e","e","e"]
-- [Multiple 4 "a",Single "b",Multiple 2 "c",Multiple 2 "a",Single "d",Multiple 4 "e"]
data MyEncoded a = Single a | Multiple Int a deriving (Show)
myModifiedEncode :: Eq a => [a] -> [MyEncoded a]
myModifiedEncode = map f . myEncode
    where
        f (1, x) = Single x
        f (n, x) = Multiple n x

-- Problem 12. Decode a Run-Length Encoded List
-- >>> myModifiedDecode [Multiple 4 "a",Single "b",Multiple 2 "c",Multiple 2 "a",Single "d",Multiple 4 "e"]
-- ["a","a","a","a","b","c","c","a","a","d","e","e","e","e"]
myModifiedDecode :: Eq a => [MyEncoded a] -> [a]
myModifiedDecode = concatMap f
    where
        f (Single x) = [x]
        f (Multiple n x) = replicate n x

-- Problem 13. Run-Length Encoding of a List (Direct Solution)
-- >>> myModifiedEncodeDirect ["a","a","a","a","b","c","c","a","a","d","e","e","e","e"]
-- [Multiple 4 "a",Single "b",Multiple 2 "c",Multiple 2 "a",Single "d",Multiple 4 "e"]
--
myModifiedEncodeDirect :: Eq a => [a] -> [MyEncoded a]
myModifiedEncodeDirect = reverse . foldl f []
    where
        f [] y = [Single y]
        f xs'@((Single x):xs) y
            | x == y = Multiple 2 x:xs
            | otherwise = Single y:xs'
        f xs'@((Multiple n x):xs) y
            | x == y = Multiple (n+1) x:xs
            | otherwise = Single y:xs'

-- Problem 14. Duplicate the Elements of a List
-- >>> myDuplicate ["a", "b", "c", "d"]
-- ["a","a","b","b","c","c","d","d"]
myDuplicate :: [a] -> [a]
myDuplicate = concatMap (\x -> [x, x])

-- Problem 15. Replicate the Elements of a List a Given Number of Times
-- >>> myNDuplicate ["a", "b", "c"] 3
-- ["a","a","a","b","b","b","c","c","c"]
myNDuplicate :: [a] -> Int -> [a]
myNDuplicate xs n = concatMap (replicate n) xs

-- Problem 16. Drop Every N'th Element From a List
-- >>> myNthDrop ["a", "b", "c", "d", "e", "f", "g", "h", "i", "j"] 3
-- ["a","b","d","e","g","h","j"]
myNthDrop :: [a] -> Int -> [a]
myNthDrop xs n = reverse $ fst $ foldl f ([], 1) xs
    where
        f (ys, acc) x
            | acc `mod` n == 0 = (ys, acc + 1)
            | otherwise = (x : ys, acc + 1)

-- Problem 17. Split a List Into Two Parts; The Length of the First Part Is Given
-- >>> mySplitParts ["a", "b", "c", "d", "e", "f", "g", "h", "i", "j"] 3
-- (["a","b","c"],["d","e","f","g","h","i","j"])
-- >>> mySplitParts ["a", "b", "c", "d"] 5
-- (["a","b","c","d"],[])
mySplitParts :: [a] -> Int -> ([a], [a])
mySplitParts xs n = fst $ foldl f (([], []), 0) xs
    where
        f ((ls, rs), acc) x
            | acc < n = ((ls ++ [x], rs), acc + 1)
            | otherwise = ((ls, rs ++ [x]), acc + 1)

-- Problem 18. Extract a Slice From a List
-- >>> mySlice ["a", "b", "c", "d", "e", "f", "g", "h", "i", "j"] 2 6
-- ["c","d","e","f","g"]
--
mySlice :: [a] -> Int -> Int -> [a]
mySlice xs s e = snd $ mySplitParts (fst $ mySplitParts xs (e + 1)) s

-- Problem 19. Rotate a List N Places to the Left
-- >>> myRotate ["a", "b", "c", "d", "e", "f", "g", "h"] 3
-- ["d","e","f","g","h","a","b","c"]
--
myRotate :: [a] -> Int -> [a]
myRotate xs n = myDrop n xs ++ myTake n xs
    where
        myTake :: Int -> [a] -> [a]
        myTake 0 _ = []
        myTake _ [] = []
        myTake m (y:ys) = y : myTake (m-1) ys
        myDrop :: Int -> [a] -> [a]
        myDrop 0 ys = ys
        myDrop _ [] = []
        myDrop m (_:ys) = myDrop (m-1) ys


-- Problem 20. Remove the K'th Element From a List
-- >>> removeAt 1 ["a", "b", "c", "d"]
-- ["a","c","d"]
removeAt :: Int -> [a] -> [a]
removeAt k xs = take k xs ++ drop (k+1) xs

-- Problem 21. Insert an Element at a Given Position Into a List
-- >>> insertAt "alfa" 1 ["a", "b", "c", "d"]
-- ["a","alfa","c","d"]
--
insertAt :: a -> Int -> [a] -> [a]
insertAt x k xs = take k xs ++ [x] ++ drop (k+1) xs

-- Problem 22. Create a List Containing All Integers Within a Given Range
-- >>> myRange 4 9
-- [4,5,6,7,8]
--
-- >>> myRange 9 4
-- [8,7,6,5,4]
--
myRange :: Int -> Int -> [Int]
myRange s e
    | s <= e = f s e s []
    | otherwise = reverse $ f e s e []
    where
        f x y z zs
            | y <= z = zs
            | otherwise = f x y (z+1) (zs++[z])


-- Problem 23. Extract a Given Number of Randomly Selected Elements From a List
-- >>> myRandomSelect 69 ["a", "b", "c", "d", "e", "f", "g", "h"] 3
-- ["f","g","a"]
myRandomSelect :: Int -> [a] -> Int -> [a]
myRandomSelect seed xs n =  chain choice n (xs, pureGen)
    where
        pureGen = mkStdGen seed
        choice (ys, g) =
            let (i, g') = uniformR (0, length ys - 1) g
            in (ys !! i, (removeAt i ys, g'))
        chain _ 0 _ = []
        chain f k s =
            let (a, s') = f s
            in (a : chain f (k-1) s')

-- Problem 24. Lotto: Draw N Different Random Numbers From the Set 1..M
--  >>> myLottoSelect 69 6 49
-- [6,7,35,31,22,41]
myLottoSelect :: Int -> Int -> Int -> [Int]
myLottoSelect seed n m  = myRandomSelect seed (myRange 1 m) n

-- Problem 25. Generate a Random Permutation of the Elements of a List
-- >>> myPermutation 69 ["a", "b", "c", "d", "e", "f"]
-- ["f","a","b","e","c","d"]
myPermutation :: Int -> [a] -> [a]
myPermutation seed xs = myRandomSelect seed xs (length xs)

-- Problem 26. Generate the Combinations of K Distinct Objects Chosen From the N Elements of a List
-- >>> myExtract 2 ["a", "b", "c", "d"]
-- [["a","b"],["a","c"],["a","d"],["b","c"],["b","d"],["c","d"]]
myExtract :: Int -> [a] -> [[a]]
myExtract 0 _ = [[]]
myExtract _ [] = []
myExtract k (x:xs) = map (x :) (myExtract (k-1) xs) ++ myExtract k xs

-- Problem 27. Group the Elements of a Set Into Disjoint Subsets
-- >>> myGroup ["a", "b", "c", "d"] [2, 1]
-- [[["b","c"],["a"]],[["b","d"],["a"]],[["c","d"],["a"]],[["a","c"],["b"]],[["a","d"],["b"]],[["c","d"],["b"]],[["a","b"],["c"]],[["a","d"],["c"]],[["b","d"],["c"]],[["a","b"],["d"]],[["a","c"],["d"]],[["b","c"],["d"]]]
myGroup :: Eq a => [a] -> [Int] -> [[[a]]]
myGroup xs ks = ts
    where
        ys = map (`myExtract` xs) ks
        zs = foldl (\ws y -> concatMap (\y' -> map (\w -> w ++ [y']) ws) y) [[]] ys
        ts = filter (\z -> length (nub $ concat z) == length (concat z)) zs

-- Problem 28. Sorting a List of Lists According to Length of Sublists
-- >>> lengthSort [["a", "b", "c"], ["d", "e"], ["f", "g", "h"], ["d", "e"], ["i", "j", "k", "l"], ["m", "n"], ["o"]]
-- [["o"],["d","e"],["d","e"],["m","n"],["a","b","c"],["f","g","h"],["i","j","k","l"]]
lengthSort :: Ord a => [[a]] -> [[a]]
lengthSort = sortBy (compare `on` length)

-- Problem 29. Determine Whether a Given Integer Number Is Prime
-- >>> isPrime 1
-- False
-- >>> isPrime 7
-- True
-- >>> isPrime 12
-- False
isPrime :: Int -> Bool
isPrime 1 = False
isPrime n = not $ any (\a -> n `mod` a == 0) [2..top]
    where
        top = floor $ (sqrt :: Double -> Double) $ fromIntegral n

main :: IO ()
main = print "Hello, World!"
