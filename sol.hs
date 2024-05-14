{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}
module FirstTen where

import Data.List (group, nub)
import GHC.Natural (Natural)
import GHC.TypeNats (Nat)
import Test.QuickCheck

-- 1.
myLast :: [a] -> a
myLast [] = error "empty list"
myLast [x] = x
myLast (_ : xs) = myLast xs

-- test
prop_1 xs = myLast xs == last xs

-- 2.
myInit :: [a] -> [a]
myInit [] = error "empty list"
myInit [x] = []
myInit (x : xs) = x : myInit xs

myButLast :: [a] -> a
myButLast = myLast . myInit

-- test
prop_2 xs = myButLast xs == (last . init) xs

-- 3.
elementAt :: [a] -> Int -> a
elementAt list i = elementAt' list 0
  where
    elementAt' [] _ = error "not found"
    elementAt' (x : xs) cntr | cntr == i = x | otherwise = elementAt' xs (cntr + 1)

-- test
prop_3 xs i = elementAt xs i == xs !! i where types = i :: Int

-- 4.
myLength :: [a] -> Int
myLength = foldr (const succ) 0

-- test
prop_4 xs = myLength xs == length xs

-- 5.
myReverse :: [a] -> [a]
myReverse list = foldr (\x f y -> f (x : y)) id list []

-- test
prop_5 xs = myReverse xs == reverse xs

-- 6.
isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs = xs == myReverse xs

-- 7.
data NestedList a = Elem a | List [NestedList a]

flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List nesteds) = concatMap flatten nesteds

-- 8.
compress :: Eq a => [a] -> [a]
compress [] = []
compress [x] = [x]
compress (x1 : x2 : xs) | x1 == x2 = compress (x2 : xs) | otherwise = x1 : compress (x2 : xs)

-- test
prop_8 xs = compress xs == nub xs

-- 9.
pack :: Eq a => [a] -> [[a]]
pack = foldr f []
  where
    f :: Eq a => a -> [[a]] -> [[a]]
    f x [] = [[x]]
    f x (ys : yss) = case ys of
      [] -> [[x]]
      (z : zs) -> if x == z then (x : ys) : yss else [x] : (ys : yss)

-- test
prop_9 xs = pack xs == group xs

-- 10
encode :: Eq a => [a] -> [(Int, a)]
encode = map (\x -> (myLength x, head x)) . pack

-- 11
data MaybeNE a = Multiple Int a | Single a deriving (Show)

pairToMaybeNE :: (Int, a) -> MaybeNE a
pairToMaybeNE (i, a) | i == 1 = Single a | otherwise = Multiple i a

encodeModified :: Eq a => [a] -> [MaybeNE a]
encodeModified = map pairToMaybeNE . encode

-- 12

decodeModified :: Eq a => [MaybeNE a] -> [a]
decodeModified = concatMap replicateNE
  where
    replicateNE :: MaybeNE a -> [a]
    replicateNE (Single a) = [a]
    replicateNE (Multiple i a) = foldr ($) [] ((:) a <$ [1 .. i])

-- test
prop_12 l = decodeModified (encodeModified l) == l

-- 13
encodeDirectly :: Eq a => [a] -> [MaybeNE a]
encodeDirectly = map pairToMaybeNE . encodeHelper 1
  where
    encodeHelper :: Eq a => Int -> [a] -> [(Int, a)]
    encodeHelper _ [] = []
    encodeHelper i [x] = [(i, x)]
    encodeHelper i (x1 : x2 : xs)
      | x1 == x2 = encodeHelper (i + 1) (x2 : xs)
      | otherwise = (i, x1) : encodeHelper 1 (x2 : xs)

-- test
prep_13 l = encodeDirectly l == encodeModified l

-- 14
dupli :: [a] -> [a]
dupli = foldr (\a -> (:) a . (:) a) []

-- 15
-- function that composes the input function with itself i times
compose_i :: Int -> (b -> b) -> (b -> b)
compose_i i f
  | i < 0 = error "negative number used"
  | i == 0 = f
  | otherwise = f . compose_i (i - 1) f

repli :: [a] -> Int -> [a]
repli list i = foldr (\x f y -> f (y ++ compose_i (i - 1) (x :) [])) id list []

-- another way:
repli' :: [a] -> Int -> [a]
repli' list i = foldr (\x y -> (x <$ [1 .. i]) ++ y) [] list

-- 16
dropEvery :: [a] -> Int -> [a]
dropEvery list i = map fst $ filter ((/= 0) . snd) (list `zip` ((`mod` i) <$> [1 .. length list]))

-- 17
split :: [a] -> Int -> ([a], [a])
split list i = foldr (\(x, i') (l, r) -> if i' <= i then (x : l, r) else (l, x : r)) ([], []) (list `zip` [1 .. length list])

-- 18
slice :: [a] -> Int -> Int -> [a]
slice list i1 i2 = fst <$> filter (\(_, i) -> i >= i1 && i <= i2) (list `zip` [1 .. length list])

-- 19
rotate :: [a] -> Int -> [a]
rotate list i = right ++ left
  where
    (left, right) = split list (if i < 0 then length list + i else i)

-- 20
removeAt :: Int -> [a] -> (a, [a])
removeAt i list = (r, left ++ right)
  where
    (left, r : right) = split list (i - 1)