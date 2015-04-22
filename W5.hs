module W5 where

import System.Random
import Data.List

-- Week 5:
--  - operators
--  - using typeclasses
--  - implementing typeclasses
--  - forcing/laziness
--
-- Useful type classes to know:
--  - Eq
--  - Ord
--  - Show
--  - Num
--  - Functor

-- Ex 1: hey, did you know you can implement your own
-- operators in Haskell? Implement the operator %$ that
-- combines two strings like this:
--
-- "aa" %$ "foo" ==> "aafooaa"
--
-- and the operator *! that takes a value and a number and
-- produces a list that repeats the value that many times:
--
-- True *! 3 ==> [True,True,True]

(%$) :: String -> String -> String
x %$ y = x ++ y ++ x

(*!) :: Int -> a -> [a]
n *! val = replicate n val

-- Ex 2: implement the function allEqual which returns True
-- if all values in the list are equal.
--
-- Examples:
--
-- allEqual [] ==> True
-- allEqual [1,2,3] ==> False
-- allEqual [1,1,1] ==> True
--
-- PS. check out the error message you get with your
-- implementation if you remove the Eq a => constraint from
-- the type!

-- soln: all function!
-- allEqual (x:xs) = all (==x) xs

allEqual :: Eq a => [a] -> Bool
allEqual [] = True
allEqual (x:xs) = ae x xs where
    ae y [] = True
    ae y (z:zs) = if y == z
        then ae z zs
        else False

-- Ex 3: implement the function secondSmallest that returns
-- the second smallest value in the list, or Nothing if
-- there is no such value.
--
-- Examples:
--
-- secondSmallest [1.0] ==>  Nothing
-- secondSmallest [1,1] ==>  Just 1
-- secondSmallest [5,3,7,2,3,1]  ==>  Just 2

-- soln: minimum vs sort

secondSmallest :: Ord a => [a] -> Maybe a
secondSmallest xs =
    if length xs <= 1 then Nothing
    else Just $ head $ tail $ sort xs

-- Ex 4: find how two lists differ from each other. If they
-- have different lengths, return Just "<length of list> /=
-- <length of other list>" if they have the same length,
-- find the first index i for which the elements differ, and
-- return Just "<value at index i> /= <other value at index
-- i>" if the lists are the same, return Nothing
--
-- NB! Write the type signature for findDifference your
-- self. Which type classes do you need?
--
-- Examples:
--  findDifference [True,False] [True,True]
--    ==> Just "False /= True"
--  findDifference [0,0,0] [0,0,0,0]
--    ==> Just "3 /= 4"

-- soln is better - shows how to do length checks first
-- then head checks, using guards then where

findDifference :: (Eq a, Show a) => [a] -> [a] -> Maybe String
findDifference ps qs = fd ps qs where
    myout o1 o2 = Just $ show o1 ++ " /= " ++ show o2
    fd [] [] = Nothing
    fd [] xs = myout (length ps) (length qs)
    fd xs [] = myout (length ps) (length qs)
    fd (x:xs) (y:ys)
        | (length xs) /= (length ys) = myout (length xs + 1) (length ys + 1)
        | x /= y = myout x y
        | otherwise = fd xs ys

-- Ex 5: compute the average of a list of values of the
-- Fractional class.
--
-- Hint! since Fractional is a subclass of Num, you have all
-- arithmetic operations available
--
-- Hint! you can use the function fromIntegral to convert
-- the list length to a Fractional

average :: Fractional a => [a] -> a
average xs =
    sum xs / (fromIntegral $ length xs)

-- Ex 6: define an Eq instance for the type Foo below.

data Foo = Bar | Quux | Xyzzy
  deriving Show

instance Eq Foo where
    Bar == Bar = True
    Quux == Quux = True
    Xyzzy == Xyzzy = True
    _ == _ = False

-- Ex 7: implement an Ord instance for Foo so that
-- Quux < Bar < Xyzzy

-- minimal Ord implements compare or <=
-- soln uses <=, which is more compact

instance Ord Foo where
  compare Quux  Quux  = EQ
  compare Bar   Bar   = EQ
  compare Xyzzy Xyzzy = EQ
  compare Quux Bar = LT
  compare Bar Xyzzy = LT
  compare _ _ = GT

-- Ex 8: here is a type for a 3d vector. Implement an Eq
-- instance for it.

data Vector = Vector Integer Integer Integer
  deriving Show

-- could also do this by 'deriving (Eq,Show)'

instance Eq Vector where
  (Vector p q r) == (Vector x y z) =
    (p == x) && (q == y) && (r == z)

-- Ex 9: implementa Num instance for Vector such that all the
-- arithmetic operations work componentwise.
--
-- You should probably check the docs for which methods Num
-- has!
--
-- Examples:
--
-- Vector 1 2 3 + Vector 0 1 1 ==> Vector 1 3 4
-- Vector 1 2 3 * Vector 0 1 2 ==> Vector 0 2 6
-- abs (Vector (-1) 2 (-3))    ==> Vector 1 2 3
-- signum (Vector (-1) 2 (-3)) ==> Vector (-1) 1 (-1)

-- Minimal complete definition:
-- (+), (*), abs, signum, fromInteger, (negate | (-))

instance Num Vector where
  (Vector p q r) + (Vector x y z) =
    Vector (p+x) (q+y) (r+z)
  (Vector p q r) * (Vector x y z) =
    Vector (p*x) (q*y) (r*z)
  abs (Vector x y z) =
    Vector (abs x) (abs y) (abs z)
  signum (Vector x y z) =
    Vector (signum x) (signum y) (signum z)
  fromInteger x =
    Vector x x x
  negate (Vector x y z) =
    Vector (negate x) (negate y) (negate z)

-- Ex 10: compute how many times each value in a list
-- occurs. Return the frequencies as a list of
-- (frequency,value) pairs.
--
-- Hint! feel free to use functions from Data.List
--
-- Example:
-- freqs [False,False,False,True]
--   ==> [(3,False),(1,True)]

-- adding "Ord a" - cheating?

freqs :: (Eq a, Ord a) => [a] -> [(Int,a)]
freqs xs =
    let foo = \x -> (length x, head x)
    in map foo $ group $ sort xs

-- Ex 11: implement an Eq instance for the following binary
-- tree type

-- soln: more succinct, using _
-- _ == _ = False

data ITree = ILeaf | INode Int ITree ITree
  deriving Show

instance Eq ITree where
    ILeaf == ILeaf = True
    INode x1 l1 r1 == ILeaf = False
    ILeaf == INode x2 l2 r2 = False
    INode x1 l1 r1 == INode x2 l2 r2 =
        x1 == x2 &&
        l1 == l2 &&
        r1 == r2

-- Ex 12: here is a list type parameterized over the type it
-- contains.  Implement an instance "Eq a => Eq (List a)" that
-- compares elements of the lists.

data List a = Empty | LNode a (List a)
  deriving Show

instance Eq a => Eq (List a) where
    Empty == Empty              = True
    LNode x l == LNode x' l'    = x == x' && l == l'
    _ == _                      = False

-- Ex 13: start by reading a bit about Functors. A Functor is
-- a thing you can "map" over, e.g. lists, Maybes.
--
-- Implement the function incrementAll that takes a functorial
-- value and increments each number inside by one.
--
-- Examples:
--   incrementAll [1,2,3]     ==>  [2,3,4]
--   incrementAll (Just 3.0)  ==>  Just 4.0

incrementAll :: (Functor f, Num n) => f n -> f n
incrementAll x = fmap (+1) x

-- Ex 14: below you'll find a type Result that works a bit
-- like Maybe, but there are two different types of
-- "Nothings": one with and one without an error description.
--
-- Implement the instance Functor Result

data Result a = MkResult a | NoResult | Failure String
  deriving (Show,Eq)

-- soln: use _'s for f for NoResult, Failure

instance Functor Result where
    fmap f NoResult     = NoResult
    fmap f (Failure x)  = Failure x
    fmap f (MkResult x) = MkResult (f x)

-- Ex 15: Implement the instance Functor List (for the
-- datatype List from ex 11)

instance Functor List where
    fmap _ Empty        = Empty
    fmap f (LNode x l)  = LNode (f x) (fmap f l)

-- Ex 16: Fun a is a type that wraps a function Int -> a.
-- Implement a Functor instance for it.
--
-- Figuring out what the Functor instance should do is most of
-- the puzzle.

data Fun a = Fun (Int -> a)

runFun :: Fun a -> Int -> a
runFun (Fun f) x = f x

instance Functor Fun where
    fmap g (Fun f) = Fun (g . f)

-- taken from the definition of Functor:
-- class Functor f where
--   fmap :: (a -> b) -> f a -> f b
--
--   g          is      (a -> b)
--   Fun f      is      f a

-- Ex 17: this and the next exercise serve as an introduction
-- for the next week.
--
-- The module System.Random has the typeclass RandomGen that
-- represents a random generator. The class Random is for
-- values that can be randomly generated by RandomGen.
--
-- The relevant function in System.Random is
--   random :: (Random a, RandomGen g) => g -> (a, g)
-- that takes a random generator and returns a random value,
-- and the new state of the generator (remember purity!)
--
-- Implement the function threeRandom that generates three
-- random values. You don't need to return the final state of
-- the random generator (as you can see from the return type).
--
-- NB! if you use the same generator multiple times, you get
-- the same output. Remember to use the new generator returned
-- by random.
--
-- NB! the easiest way to get a RandomGen value is the
-- function mkStdGen that takes a seed and returns a random
-- generator.
--
-- Examples:
--  *W5> threeRandom (mkStdGen 1) :: (Int,Int,Int)
--  (7917908265643496962, -1017158127812413512,i
--  -1196564839808993555)
--  *W5> threeRandom (mkStdGen 2) :: (Bool,Bool,Bool)
--  (True,True,False)

threeRandom :: (Random a, RandomGen g) => g -> (a,a,a)
threeRandom g =
    let (v1, g1) = random g
        (v2, g2) = random g1
        (v3, _)  = random g2
    in (v1,v2,v3)

-- Ex 18: given a Tree (same type as on Week 3), randomize the
-- contents of the tree.
--
-- That is, you get a RandomGen and a Tree, and you should
-- return a Tree with the same shape, but random values in the
-- Nodes.
--
-- This time you should also return the final state of the
-- RandomGen
--
-- Hint! the recursive solution is straightforward, but
-- requires careful threading of the RandomGen versions.
--
-- Examples:
--  *W5> randomizeTree (Node 0 (Node 0 Leaf Leaf) Leaf)
--  (mkStdGen 1)  :: (Tree Char, StdGen)
--  (Node '\603808' (Node '\629073' Leaf Leaf) Leaf,1054756829
--  1655838864)
--  *W5> randomizeTree (Node True Leaf Leaf) (mkStdGen 2)  ::
--  (Tree Int, StdGen)
--  (Node (-2493721835987381530) Leaf Leaf,1891679732
--  2103410263)

data Tree a = Leaf | Node a (Tree a) (Tree a)
  deriving Show

randomizeTree :: (Random a, RandomGen g) => Tree b -> g -> (Tree a,g)
randomizeTree t g = undefined
