module W2 where

-- Week 2:
--
--  * lists
--  * strings
--  * library functions for them
--  * higher order functions
--  * polymorphism
--
-- Functions you will need:
--  * head, tail
--  * take, drop
--  * length
--  * null
--  * map
--  * filter
--
-- You can ask ghci for the types of these functions with the :t
-- command:
--
--  Prelude> :t length
--  length :: [a] -> Int

import Data.List
import Data.Char

-- Ex 1: Define the constant years, that is a list of the values 1982,
-- 2004 and 2012 in this order.

years = [1982, 2004, 2012]

-- Ex 2: define the function measure that for an empty list returns -1
-- and for other lists returns the length of the list.

-- soln: null to check for empty lists, length for length of lists

measure :: [String] -> Int
measure [] = -1
measure [s] = 1
measure ss = 1 + (measure (tail ss))

-- Ex 3: define the function takeFinal, which returns the n last
-- elements of the given list.

-- soln: more concise by not separately calculating m

takeFinal :: Int -> [Int] -> [Int]
takeFinal n xs =
    drop m xs
    where
        m = (length xs) - n

-- Ex 4: remove the nth element of the given list. More precisely,
-- return a list that is identical to the given list except the nth
-- element is missing.
--
-- Note! indexing starts from 0
--
-- Examples:
-- remove 0 [1,2,3]    ==>  [2,3]
-- remove 2 [4,5,6,7]  ==>  [4,5,7]
--
-- The [a] in the type signature means "a list of any type"

remove :: Int -> [a] -> [a]
remove i xs =
    take i xs ++ drop (i+1) xs

-- Ex 5: substring i n s should return the length n substring of s
-- starting at index i.
--
-- Remember that strings are lists!

substring :: Int -> Int -> String -> String
substring i n s =
    take n (drop i s)

-- Ex 6: implement the function mymax that takes as argument a
-- measuring function (of type a -> Int) and two values (of type a).
--
-- mymax should apply the measuring function to both arguments and
-- return the argument for which the measuring function returns a
-- higher value.
--
-- Examples:
--
--  mymax (*2)   3       5      ==>  5
--  mymax length [1,2,3] [4,5]  ==>  [1,2,3]
--  mymax head   [1,2,3] [4,5]  ==>  [4,5]

mymax :: (a -> Int) -> a -> a -> a
mymax measure a b =
    if (measure a) > (measure b)
        then a
        else b

-- Ex 7: countSorted receives a list of strings and returns a count of
-- how many of the strings are in alphabetical order (i.e. how many of
-- the strings have their letters in alphabetical order)
--
-- Remember the functions length, filter and sorted

countSorted :: [String] -> Int
countSorted ss = length $ filter isSorted ss
    where isSorted xs = and $ zipWith (<=) xs (tail xs)

-- Ex 8: Implement a function funny, that
--  - takes in a list of strings
--  - returns a string
--    - that contains all input words of length over 5
--    - ... combined into one string
--    - ... separated with spaces
--    - ... and converted to upper case!
--
-- These functions will help:
--  - toUpper :: Char -> Char   from the module Data.Char
--  - intercalate               from the module Data.List

funny :: [String] -> String
funny ss = map toUpper $ intercalate " " $ filter (\x -> length x > 5) ss

-- Ex 9: implement quicksort. Quicksort is a recursive sorting
-- algorithm that works like this.
--
--  - The empty list is the base case of the recursion: it is already sorted
--  - From a nonempty list, the first element is chosen to be the "pivot", and
--    - the elements smaller than pivot are gathered into a list
--    - the elements smaller than larger or equal to the pivot are gathered
--    - these two lists are sorted using recursion
--    - finally the small elements, the pivot and the large elements
--      are combined into one sorted list
--
-- PS. yes if you want to nit-pick this isn't really quicksort :)

quicksort :: [Int] -> [Int]
quicksort [] = []
quicksort (hd:tl) = quicksort(left hd tl) ++ [hd] ++ quicksort(right hd tl)
    where   left x xs = [x' | x' <- xs, x' <= x]
            right x xs = [x' | x' <- xs, x' > x]

-- Ex 10: powers k max should return all the powers of k that are less
-- than or equal to max. For example:
--
-- powers 2 5 ==> [1,2,4]
-- powers 3 30 ==> [1,3,9,27]
-- powers 2 2 ==> [1,2]
--
-- Hints:
--   * n^max > max
--   * the function takeWhile

-- soln: takeWhile (<=max) $ map (n^) [0..max]

powers :: Int -> Int -> [Int]
powers n max =
    [n ^ y | y <- xs]
    where xs = takeWhile (\ x -> n^x <= max) [0..]

-- Ex 11: implement a search function that takes an updating function,
-- a checking function and an initial value. Search should repeatedly
-- apply the updating function to the initial value until a value is
-- produced that passes the checking function. This value is then
-- returned.
--
-- Examples:
--
--   search (+1) even 0   ==>   0
--
--   search (+1) (>4) 0   ==>   5
--
--   let check [] = True
--       check ('A':xs) = True
--       check _ = False
--   in search tail check "xyzAvvt"
--     ==> Avvt

search :: (a->a) -> (a->Bool) -> a -> a
search update check initial =
    let fn x = if check x then x else fn $ update x
    in fn initial

-- Ex 12: given numbers n and k, build the list of numbers n,n+1..k.
-- Use recursion and the : operator to build the list.

-- soln: fromTo n k = if n>k then [] else n:fromTo (n+1) k

fromTo :: Int -> Int -> [Int]
fromTo n k = go n k [] where
    go n k xs
        | n > k = []
        | n == k = k : xs
        | otherwise = go n (k-1) $ k : xs

-- Ex 13: given i, build the list of sums [1, 1+2, 1+2+3, .., 1+2+..+i]
--
-- Ps. you'll probably need a recursive helper function

sums :: Int -> [Int]
sums i = [ sum [1..x] | x <- [1..i] ]

-- Ex 14: using list pattern matching and recursion, define a function
-- mylast that returns the last value of the given list. For an empty
-- list, a provided default value is returned.
--
-- Examples:
--   mylast 0 [] ==> 0
--   mylast 0 [1,2,3] ==> 3

-- soln: 2nd def can be skipped: mylast _ (x:xs) = mylast x xs

mylast :: a -> [a] -> a
mylast def [] = def
mylast _   (x:xs) = mylast x xs

-- Ex 15: define a function that checks if the given list is in
-- increasing order. Use recursion and pattern matching. Don't use any
-- library list functions.

sorted :: [Int] -> Bool
sorted xs = and [ x<=y | (x,y) <- zip xs (tail xs) ]

-- Ex 16: compute the partial sums of the given list like this:
--
--   sumsOf [a,b,c]  ==>  [a,a+b,a+b+c]
--   sumsOf [a,b]    ==>  [a,a+b]
--   sumsOf [a]      ==>  [a]
--   sumsOf []       ==>  []

sumsOf :: [Int] -> [Int]
sumsOf xs = sumsOf' 1 $ length xs where
    sumsOf' n l
        | n <= l = sum (take n xs) : sumsOf' (n+1) l
        | otherwise = []

-- Ex 17: define the function mymaximum that takes a list and a
-- comparing function of type a -> a -> Ordering and returns the
-- maximum value of the list, according to the comparing function.
--
-- For an empty list the given default value is returned.
--
-- Examples:
--   mymaximum compare (-1) [] ==> -1
--   mymaximum compare (-1) [1,3,2] ==> 3
--   let comp 0 0 = EQ
--       comp _ 0 = LT
--       comp 0 _ = GT
--       comp x y = compare x y
--   in mymaximum comp 1 [1,4,6,100,0,3]
--     ==> 0

mymaximum :: (a -> a -> Ordering) -> a -> [a] -> a
mymaximum _   def [] = def
mymaximum cmp _   (x:xs) = go cmp x xs where
    go _   largest [] = largest
    go cmp largest (y:ys) =
        if largest `cmp` y == GT
            then go cmp largest ys
            else go cmp y       ys

-- Ex 18: define a version of map that takes a two-argument function
-- and two lists. Example:
--   map2 f [x,y,z,w] [a,b,c]  ==> [f x a, f y b, f z c]
--
-- Use recursion and pattern matching.
--
-- Ps. this function is in the Haskell Prelude but under a different
-- name.

map2 :: (a -> b -> c) -> [a] -> [b] -> [c]
map2 _ as [] = []
map2 _ [] bs = []
map2 f (a:as) (b:bs) =
    (f a b) : map2 f as bs

-- Ex 19: in this exercise you get to implement an interpreter for a
-- simple language. The language controls two counters, A and B, and
-- has the following commands:
--
-- incA -- increment counter A by one
-- incB -- likewise for B
-- decA -- decrement counter A by one
-- decB -- likewise for B
-- printA -- print value in counter A
-- printB -- print value in counter B
--
-- The interpreter will be a function of type [String] -> [String].
-- Its input is a list of commands, and its output is a list of the
-- results of the print commands in the input.
--
-- Both counters should start at 0.
--
-- Examples:
--
-- interpreter ["incA","incA","incA","printA","decA","printA"] ==> ["3","2"]
-- interpreter ["incA","incB","incB","printA","printB"] ==> ["1","2"]
--
-- Surprise! after you've implemented the function, try running this in GHCi:
--     interact (unlines . interpreter . lines)
-- after this you can enter commands on separate lines and see the
-- responses to them
--
-- Unfortunately the surprise might not work if you've implemented
-- your interpreter correctly but weirdly :(

interpreter :: [String] -> [String]
interpreter []       = []
interpreter commands = go 0 0 commands [] where
    go _ _ [] result = result
    go a b (cmd:cmds) result
        | cmd == "incA" = go (a+1) b cmds result
        | cmd == "incB" = go a (b+1) cmds result
        | cmd == "decA" = go (a-1) b cmds result
        | cmd == "decB" = go a (b-1) cmds result
        | cmd == "printA" = go a b cmds $ result ++ [show a]
        | cmd == "printB" = go a b cmds $ result ++ [show b]
        | otherwise = go a b cmds $ result ++ ["BAD"]

-- Ex 20: write a function that finds the n first squares (numbers of
-- the form x*x) that start and end with the same digit.
--
-- Example: squares 9 ==> [1,4,9,121,484,676,1521,1681,4624]
--
-- Remember, the function show transforms a number to a string.

-- soln: more compact
-- squares n = take n . filter (\x -> head (show x) == last (show x)) $ map (\x -> x*x) [1..]

squares :: Int -> [Integer]
squares 0 = []
squares n =
    let
        ok x = head (show x) == last (show x)
        ns = [ x*x | x <- [1..], ok $ x*x ]
    in
        take n ns
