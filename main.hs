module Main where

myLast :: [a] -> Maybe a
myLast [] = Nothing
myLast [x] = Just x
myLast (_:xs) = myLast xs

myTwoLast :: [a] -> Maybe (a, a)
myTwoLast [] = Nothing
myTwoLast [_] = Nothing
myTwoLast [x, y] = Just (x, y)
myTwoLast (_:xs) = myTwoLast xs

myAt :: Int -> [a] -> Maybe a
myAt _ [] = Nothing
myAt 0 (x:xs) = Just x
myAt n (x:xs) = myAt (n - 1) xs

myLength :: [a] -> Int
myLength = foldr (const (+1)) 0

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

myPalindrome :: Eq a => [a] -> Bool
myPalindrome xs = xs == myReverse xs

main :: IO ()
main = undefined

