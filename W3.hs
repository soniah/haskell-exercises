module W3 where

import qualified Data.Maybe as Foo

-- Week 3:
--   * built-in datatypes
--   * custom datatypes
--   * folds
--
-- Useful functions / types:
--   * Maybe

-- Ex 1: implement safe integer devision, that is, a function that
-- returns Just result normally, but Nothing if the divisor is zero.
--
-- Remember that integer division can be done with the div function.

safeDiv :: Integer -> Integer -> Maybe Integer
safeDiv _ 0 = Nothing
safeDiv x y = Just $ x `div` y

-- Ex 2: another variant of safe division. This time a succesful
-- division should be returned as
--   Right result
-- whereas a division by zero should return
--   Left "1234/0"
-- (replace 1234 with the value of x).

eitherDiv :: Integer -> Integer -> Either String Integer
eitherDiv x 0 = Left  $ show x ++ "/" ++ show 0
eitherDiv x y = Right $ x `div` y

-- Ex 3: implement the function mapMaybe, which works a bit like a
-- combined map & filter.
--
-- mapMaybe is given a list ([a]) and a function of type a -> Maybe b.
-- This function is called for all values in the list. If the function
-- returns Just x, x will be in the result list. If the function
-- returns Nothing, nothing gets added to the result list.
--
-- Examples:
--
-- let f x = if x>0 then Just (2*x) else Nothing
-- in mapMaybe f [0,1,-1,4,-2,2]
--   ==> [2,8,4]
--
-- mapMaybe Just [1,2,3]
--   ==> [1,2,3]
--
-- mapMaybe (\x -> Nothing) [1,2,3]
--   ==> []

mapMaybe :: (a -> Maybe b) -> [a] -> [b]
mapMaybe f xs = catMaybes $ map f xs where
    catMaybes ms = [ x | Just x <- ms ]

-- Ex 4: define the function classify that takes a list of Either a b
-- values and returns a list of the Left values and a list of the
-- Right values.
--
-- PS. This function can be found from the standard library under the
-- name partitionEithers. Don't use the library implementation or any
-- other functions from the Data.Maybe module.
--
-- Example:
--  classify [Left 1, Right True, Left 0, Right False]
--     ==> ([1,0],[True,False])

classify :: [Either a b] -> ([a],[b])
classify es = go [] [] es where
    go lefts rights [] = (lefts, rights)
    go lefts rights (x:xs) = case x of
        Left y -> go (lefts ++ [y]) rights xs
        Right y -> go lefts (rights ++ [y]) xs

-- Ex 5: define a datatype Person, which should contain the age (an
-- Int) and the name (a String) of a person.
--
-- Also define a Person value fred, and the functions getAge, getname,
-- setAge and setName (see below).

data Person = Person Int String
  deriving Show

-- fred is a person whose age is 90 and name is Fred
fred :: Person
fred = Person 90 "Fred"

-- getName returns the name of the person
getName :: Person -> String
getName (Person _ name) = name

-- getAge returns the age of the person
getAge :: Person -> Int
getAge (Person age _) = age

-- setName takes a person and returns a new person with the name changed
setName :: String -> Person -> Person
setName name (Person age _) = Person age name

-- setAge does likewise for age
setAge :: Int -> Person -> Person
setAge age (Person _ name) = Person age name

-- Ex 6: define a datatype TwoCounters which contains two Int
-- counters. Also define the functions below for operating a
-- TwoCounters.
--
-- Examples:
--
-- getA (incA (incA zeros))
--   ==> 2
-- getB (incB (incA zeros))
--   ==> 1

data TwoCounters = TwoCounters Int Int

-- zeros is a TwoCounters value with both counters initialized to 0
zeros :: TwoCounters
zeros = TwoCounters 0 0

-- getA returns the value of the "A" counter
getA :: TwoCounters -> Int
getA (TwoCounters a _) = a

-- getB returns the value of the "B" counter
getB :: TwoCounters -> Int
getB (TwoCounters _ b) = b

-- incA increases the value of the "A" counter by one
incA :: TwoCounters -> TwoCounters
incA (TwoCounters a b) = TwoCounters (succ a) b

-- incB does likewise for the "B" counter
incB :: TwoCounters -> TwoCounters
incB (TwoCounters a b) = TwoCounters a (succ b)

-- Ex 7: define a datatype UpDown that represents a counter that can
-- either be in incresing or decreasing mode. Also implement the
-- functions zero, toggle, tick and get below.
--
-- NB! Define _two_ constructors for your datatype, in other words,
-- the definition should be of the form
--   data UpDown = A something | B foobar
--
-- Examples:
--
-- get (tick zero)
--   ==> 1
-- get (tick (tick zero))
--   ==> 2
-- get (tick (tick (toggle (tick zero))))
--   ==> -1

data UpDown = Upper Int | Downer Int

-- zero is an increasing counter with value 0
zero :: UpDown
zero = Upper 0

-- get returns the counter value
get :: UpDown -> Int
get (Upper x) = x
get (Downer x) = x

-- tick increases an increasing counter by one or decreases a
-- decreasing counter by one
tick :: UpDown -> UpDown
tick (Upper x) = Upper (x+1)
tick (Downer x) = Downer (x-1)

-- toggle changes an increasing counter into a decreasing counter and
-- vice versa
toggle :: UpDown -> UpDown
toggle (Upper x) = Downer x
toggle (Downer x) = Upper x

-- !!!!!
-- The next exercises use the binary tree type defined like this:

data Tree a = Leaf | Node a (Tree a) (Tree a)
            deriving (Show, Eq)

-- Ex 8: implement the function valAtRoot which returns the value at
-- the root (top-most node) of the tree. The return value is Maybe a
-- because the tree might be empty (i.e. just a Leaf)

valAtRoot :: Tree a -> Maybe a
valAtRoot (Node x _ _) = Just x
valAtRoot _ = Nothing

-- Ex 9: compute the size of a tree, that is, the number of Node
-- constructors in it

treeSize :: Tree a -> Int
treeSize Leaf = 0
treeSize (Node _ t1 t2) = 1 + treeSize t1 + treeSize t2

-- Ex 10: get the leftmost value in the tree. The return value is
-- Maybe a because the tree might be empty.
--
-- The leftmost value means the value that is reached by going to the
-- left child of the current Node for as long as possible.
--
-- Examples:
--
-- leftest Leaf
--   ==> Nothing
-- leftest (Node 1 (Node 2 (Node 3 Leaf Leaf) Leaf) Leaf)
--   ==> Just 3
-- leftest (Node 1 (Node 2 Leaf (Node 3 Leaf Leaf)) (Node 4 Leaf Leaf))
--   ==> Just 2

leftest :: Tree a -> Maybe a
leftest x = Foo.listToMaybe $ myflatten x where
    myflatten Leaf = []
    myflatten (Node y t1 t2) = myflatten t1 ++ [y] ++ myflatten t2

-- Ex 11: implement map for trees.
--
-- Examples:
--
-- mapTree (+1) Leaf  ==>  Leaf
-- mapTree (+2) (Node 0 (Node 1 Leaf Leaf) (Node 2 Leaf Leaf))
--   ==> (Node 2 (Node 3 Leaf Leaf) (Node 4 Leaf Leaf))

mapTree :: (a -> b) -> Tree a -> Tree b
mapTree _ Leaf = Leaf
mapTree f (Node x left right) =
    Node (f x) (mapTree f left) (mapTree f right)

-- Ex 12: insert the given value into the leftmost possible place. You
-- need to return a new tree since the function is pure.
--
-- Example:
-- insertL 0 Leaf
--   ==> Node 0 Leaf Leaf
-- insertL 0 (Node 1 Leaf Leaf)
--   ==> Node 1 (Node 0 Leaf Leaf) Leaf)
--
-- insertL 0 (Node 1
--             (Node 2
--               Leaf
--               (Node 3 Leaf Leaf))
--             (Node 4 Leaf Leaf))
--
--        ==> Node 1
--             (Node 2
--               (Node 0 Leaf Leaf)
--               (Node 3 Leaf Leaf))
--             (Node 4 Leaf Leaf)


insertL :: a -> Tree a -> Tree a
insertL x Leaf = Node x Leaf Leaf
insertL x (Node y left right) =
    Node y (insertL x left) right

-- Ex 13: implement the function measure, that takes a tree and
-- returns a tree with the same shape, but with the value at every
-- node being the size of the subtree starting at that node.
--
-- Tree size is defined as in Ex 9
--
-- Examples:
--
-- measure (Node 'a' Leaf Leaf)
--  ==> Node 1 Leaf Leaf
-- measure (Node 'a' (Node 'b' Leaf Leaf) Leaf)
--  ==> Node 2 (Node 1 Leaf Leaf) Leaf
-- measure (Node 0 (Node 0 Leaf Leaf) Leaf)
--  ==> Node 2 (Node 1 Leaf Leaf) Leaf
-- measure (Node 0 (Node 0 Leaf Leaf)
--                 (Node 0 (Node 0 Leaf Leaf)
--                         (Node 0 Leaf
--                                 (Node 0 Leaf Leaf))))
--      ==> Node 6 (Node 1 Leaf Leaf)
--                 (Node 4 (Node 1 Leaf Leaf)
--                         (Node 2 Leaf
--                                 (Node 1 Leaf Leaf)))


-- soln: more efficient, as it recursively calculates subnodes, then
-- sums subnode values to produce node value. Mine re-traverses subtree
-- on every node.

measure :: Tree a -> Tree Int
measure Leaf = Leaf
measure (Node x left right) =
    Node size (measure left) (measure right) where
        size = treeSize (Node x left right)

-- Ex 14: the standard library function
--   foldr :: (a -> b -> b) -> b -> [a] -> b
-- is used to "collapse" a list to a single value, like this:
--   foldr f start [x,y,z,w]
--     ==> f x (f y (f z (f w start)
--
-- Implement the functions sumf and lengthf so that mysum computes the
-- sum of the values in the list and mylength computes the length of
-- the list.
--
-- DON'T change the definitions of mysum and mylength, only implement
-- sumf and lengtf appropriately.

mysum :: [Int] -> Int
mysum is = foldr sumf 0 is

sumf :: Int -> Int -> Int
sumf x y = x + y

mylength :: [a] -> Int
mylength xs = foldr lengthf 0 xs

lengthf :: a -> Int -> Int
lengthf x y = y + 1

-- Ex 15: implement the function foldTree that works like foldr, but
-- for Trees.
--
-- Example:
--   foldTree f l (Node 3 Leaf Leaf)
--     ==> f 3 l l
--   foldTree f l (Node 'a' (Node 'b' (Node 'c' Leaf Leaf)
--                                    Leaf)
--                          (Node 'd' Leaf Leaf))
--     ==> f (f 'a' (f 'b' (f 'c' l l)
--                         l)
--                  (f 'd' l l))
--
-- Once you've implemented foldTree correctly, the functions treeSum
-- and treeLeaves below work correctly.

sumt :: Int -> Int -> Int -> Int
sumt x y z = x+y+z

-- Sum of numbers in the tree
treeSum :: Tree Int -> Int
treeSum t = foldTree sumt 0 t

leaft :: a -> Int -> Int -> Int
leaft x y z = y+z

-- Number of leaves in the tree
treeLeaves :: Tree a -> Int
treeLeaves t = foldTree leaft 1 t

foldTree :: (a -> b -> b -> b) -> b -> Tree a -> b
foldTree f z Leaf = z
foldTree f z (Node x left right) =
   f x (foldTree f z left) (foldTree f z right)

-- Ex 16: You'll find a Color datatype below. It has the three basic
-- colours Red, Green and Blue, and two color transformations, Mix and
-- Darken.
--
-- Mix means the sum of the two colors.
--
-- Darken means darkening the color. The Double value tells how much
-- the color is darkened. 0.0 means no change and 1.0 means full
-- darkening, i.e. black.
--
-- Implement the function rgb :: Color -> [Double] that returns a list
-- of length three that represents the rgb value of the given color.
--
-- Examples:
--
-- rgb Red   ==> [1,0,0]
-- rgb Green ==> [0,1,0]
-- rgb Blue  ==> [0,0,1]
--
-- rgb (Mix Red Green)                ==> [1,1,0]
-- rgb (Mix Red (Mix Red Green))      ==> [1,1,0]
-- rgb (Darken 0.2 Red)               ==> [0.8,0,0]
-- rgb (Darken 0.2 (Darken 0.2 Red))  ==> [0.64,0,0]
-- rgb (Mix (Darken 0.4 Red) (Darken 0.4 Red)) ==> [1,0,0]
-- rgb (Mix (Darken 0.6 Red) (Darken 0.6 Red)) ==> [0.8,0,0]
--
-- NB! Mix should saturate at 1.0

data Color = Red | Green | Blue | Mix Color Color | Darken Double Color
  deriving Show

-- soln:
-- zipWith - can combine z1 and f1

rgb :: Color -> [Double]
rgb Red = [1,0,0]
rgb Green = [0,1,0]
rgb Blue = [0,0,1]
rgb (Mix c1 c2) =
    let z1 = zip (rgb c1) (rgb c2)
        f1 (x,y) = bound $ x + y
    in map f1 z1
rgb (Darken d1 c1) =
    let darken x = bound $ x - (d1 * x)
    in  map darken (rgb c1)

-- keep x within bounds of 0 and 1
bound :: (Ord a, Num a) => a -> a
bound x
    | x < 0 = 0
    | x > 1 = 1
    | otherwise = x
