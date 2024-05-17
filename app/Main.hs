module Main where

import Prelude hiding (last, length, reverse)

last :: [a] -> Maybe a
last [] = Nothing
last [x] = Just x
last (_:xs) = last xs

lastTwo :: [a] -> Maybe (a, a)
lastTwo [] = Nothing
lastTwo [_] = Nothing
lastTwo [x, y] = Just (x, y)
lastTwo (_:xs) = lastTwo xs

nth :: [a] -> Int -> Maybe a
nth [] _ = Nothing
nth (x:_) 0 = Just x
nth (_:xs) n = nth xs (n - 1)

length :: [a] -> Int
length = go 0
    where
        go :: Int -> [a] -> Int
        go n [] = n
        go n (_:xs) = go (n + 1) xs

reverse :: [a] -> [a]
reverse [] = []
reverse (x:xs) = (reverse xs) ++ [x]

isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs = xs == reverse xs

data Node a = One a | Many [Node a] deriving Show

flatten :: Node a -> [a]
flatten (One a) = [a]
flatten (Many xs) = concatMap flatten xs

compress :: Eq a => [a] -> [a]
compress xs = map head $ pack xs

pack :: Eq a => [a] -> [[a]]
pack = go []
    where
        go acc [] = acc
        go acc xs@(x:_) = let (g, xs') = break (/= x) xs in go (acc ++ [g]) xs'

encode :: Eq a => [a] -> [(Int, a)]
encode = map (\x -> (length x, head x)) . pack

data Rle a = ROne a | RMany (Int, a) deriving Show

encode' :: Eq a => [a] -> [Rle a]
encode' = map rle . encode
    where
        rle :: (Int, a) -> Rle a
        rle (1, a) = ROne a
        rle (n, a) = RMany (n, a)

decode :: [Rle a] -> [a]
decode [] = []
decode (ROne a:xs) = a : decode xs
decode (RMany (n, a):xs) = replicate n a ++ decode xs

duplicate :: [a] -> [a]
duplicate [] = []
duplicate (x:xs) = x : x : duplicate xs

replicateList :: [a] -> Int -> [a]
replicateList [] _ = []
replicateList (x:xs) n = replicate n x ++ replicateList xs n

drop' :: [a] -> Int -> [a]
drop' [] _ = []
drop' xs n = take (n - 1) xs ++ drop' (drop n xs) n

main :: IO ()
main = do
    putStrLn "# Tail of a List"
    print (last ["a", "b", "c"])
    print ((last :: [Int] -> Maybe Int) [])
    putStrLn "# Last Two Elements of a List"
    print (lastTwo ["a", "b", "c"])
    print ((lastTwo :: [Int] -> Maybe (Int, Int)) [1])
    print ((lastTwo :: [Int] -> Maybe (Int, Int)) [])
    putStrLn "# N'th Element of a List"
    print (nth ["a", "b", "c"] 1)
    print (nth ["a", "b", "c"] 4)
    print ((nth :: [Int] -> Int -> Maybe Int) [] 4)
    putStrLn "# Length of a List"
    print (length ["a", "b", "c"])
    print (length [])
    putStrLn "# Reverse a List"
    print (reverse ["a", "b", "c"])
    putStrLn "# Palindrome"
    print (isPalindrome ["x", "a", "m", "a", "x"])
    print (isPalindrome ["a", "b"])
    putStrLn "# Flatten a List"
    print (flatten (One "a"))
    print (flatten (Many [One "a", Many [One "b", One "c"], One "d"]))
    putStrLn "# Eliminate Duplicates"
    print (compress ["a", "a", "a", "a", "b", "c", "c", "a", "a", "d", "e", "e", "e", "e"])
    putStrLn "# Pack Consecutive Duplicates"
    print (pack ["a", "a", "a", "a", "b", "c", "c", "a", "a", "d", "e", "e", "e", "e"])
    putStrLn "# Run-Length Encoding"
    print (encode ["a", "a", "a", "a", "b", "c", "c", "a", "a", "d", "e", "e", "e", "e"])
    putStrLn "# Modified Run-Length Encoding"
    print (encode' ["a", "a", "a", "a", "b", "c", "c", "a", "a", "d", "e", "e", "e", "e"])
    putStrLn "# Decode a Run-Length Encoded List"
    print (decode [RMany (4,"a"), ROne "b", RMany (2,"c"), RMany (2,"a"), ROne "d", RMany (4,"e")])
    putStrLn "#  Duplicate the Elements of a List"
    print (duplicate ["a", "b", "c", "c", "d"])
    putStrLn "# Replicate the Elements of a List a Given Number of Times"
    print (replicateList ["a", "b", "c", "c", "d"] 3)
    putStrLn "# Drop Every N'th Element From a List"
    print (drop' ["a", "b", "c", "d", "e", "f", "g", "h", "i", "j"] 3)
