module OneToTen where

-- 1.
myLast :: [a] -> a
myLast [] = error "empty list"
myLast [x] = x
myLast (_ : xs) = myLast xs

-- 2.
myInit :: [a] -> [a]
myInit [] = error "empty list"
myInit [x] = []
myInit (x : xs) = x : myInit xs

myButLast :: [a] -> a
myButLast = myLast . myInit

-- 3.
elementAt :: [a] -> Int -> a
elementAt list i = elementAt' list 0
  where
    elementAt' [] _ = error "not found"
    elementAt' (x : xs) cntr | cntr == i = x | otherwise = elementAt' xs (cntr + 1)

-- 4.
myLength :: [a] -> Int
myLength = foldr (const succ) 0

-- 5.
myReverse :: [a] -> [a]
myReverse list = foldr (\x f y -> f (x : y)) id list []

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

-- 9.
pack :: Eq a => [a] -> [[a]]
pack = foldr f []
  where
    f :: Eq a => a -> [[a]] -> [[a]]
    f x [] = [[x]]
    f x (ys : yss) = case ys of
      [] -> [[x]]
      (z : zs) -> if x == z then (x : ys) : yss else [x] : (ys : yss)

-- 10
encode :: Eq a => [a] -> [(Int, a)]
encode = map (\x -> (myLength x, head x)) . pack