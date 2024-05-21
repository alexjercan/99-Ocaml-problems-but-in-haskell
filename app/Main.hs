module Main where

import Prelude hiding (last, length, reverse)
import System.Random (uniformR, mkStdGen, RandomGen)
import Data.List (unfoldr)
import Data.Time (getCurrentTime, UTCTime (utctDayTime))
import Data.Time.Clock (diffTimeToPicoseconds)

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

split :: [a] -> Int -> ([a], [a])
split xs n = (take n xs, drop n xs)

slice :: [a] -> Int -> Int -> [a]
slice xs i k = take (k - i + 1) $ drop i xs

rotate :: [a] -> Int -> [a]
rotate xs n = b ++ a
    where (a, b) = split xs n

removeAt :: Int -> [a] -> [a]
removeAt k xs = (take k xs) ++ (drop (k + 1) xs)

insertAt :: a -> Int -> [a] -> [a]
insertAt e n xs = a ++ [e] ++ b
    where (a, b) = split xs n

range :: Int -> Int -> [Int]
range a b = if a > b then dec a b else inc a b
    where
        dec a' b' = if a' < b' then [] else a' : dec (a' - 1) b'
        inc a' b' = if a' > b' then [] else a' : inc (a' + 1) b'

randSelect :: Int -> [a] -> Int -> [a]
randSelect seed xs n = take n $ unfoldr go (mkStdGen seed, xs)
    where
        go :: RandomGen g => (g, [a]) -> Maybe (a, (g, [a]))
        go (_, []) = Nothing
        go (g, ys) = let (i, g') = uniformR (0, length ys - 1) g in Just (ys !! i, (g', removeAt i ys))

lottoSelect :: Int -> Int -> Int -> [Int]
lottoSelect seed k n = randSelect seed (range 1 n) k

permutation :: Int -> [a] -> [a]
permutation seed xs = randSelect seed xs $ length xs

extract :: Int -> [a] -> [[a]]
extract k _ | k <= 0 = [[]]
extract _ [] = []
extract k (x:xs) = let withX = map (x:) $ extract (k - 1) xs
                       withoutX = extract k xs
                   in withX ++ withoutX

main :: IO ()
main = do
    time <- (fromInteger . diffTimeToPicoseconds . utctDayTime) <$> getCurrentTime

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
    putStrLn "# Split a List Into Two Parts; The Length of the First Part Is Given"
    print (split ["a", "b", "c", "d", "e", "f", "g", "h", "i", "j"] 3)
    print (split ["a", "b", "c", "d"] 5)
    putStrLn "# Extract a Slice From a List"
    print (slice ["a", "b", "c", "d", "e", "f", "g", "h", "i", "j"] 2 6)
    putStrLn "# Rotate a List N Places to the Left"
    print (rotate ["a", "b", "c", "d", "e", "f", "g", "h", "i", "j"] 3)
    putStrLn "# Remove the K'th Element From a List"
    print (removeAt 1 ["a", "b", "c", "d", "e", "f", "g", "h", "i", "j"])
    putStrLn "# Insert an Element at a Given Position Into a List"
    print (insertAt "alpha" 1 ["a", "b", "c", "d", "e", "f", "g", "h", "i", "j"])
    putStrLn "# Create a List Containing All Integers Within a Given Range"
    print (range 4 9)
    print (range 9 4)
    putStrLn "# Extract a Given Number of Randomly Selected Elements From a List"
    print (randSelect time ["a", "b", "c", "d", "e", "f", "g", "h", "i", "j"] 3)
    putStrLn "# Lotto: Draw N Different Random Numbers From the Set 1..M"
    print (lottoSelect time 6 49)
    putStrLn "# Generate a Random Permutation of the Elements of a List"
    print (permutation time ["a", "b", "c", "d", "e", "f"])
    putStrLn "# Generate the Combinations of K Distinct Objects Chosen From the N Elements of a List"
    print (extract 2 ["a", "b", "c", "d", "e", "f", "g", "h", "i", "j"])
