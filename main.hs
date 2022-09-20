module Main where

import Control.Arrow

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

main :: IO ()
main = undefined
