import Data.List
import OneToTen
import Test.QuickCheck

-- simple tests
prop_1 xs = myLast xs == last xs

prop_2 xs = myButLast xs == (last . init) xs

prop_3 xs i = elementAt xs i == xs !! i where types = i :: Int

prop_4 xs = myLength xs == length xs

prop_5 xs = myReverse xs == reverse xs

prop_6 xs = isPalindrome xs == (reverse xs == xs)

prop_8 xs = compress xs == nub xs

prop_9 xs = pack xs == group xs
