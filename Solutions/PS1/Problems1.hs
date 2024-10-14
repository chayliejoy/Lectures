module Problems1 where

import Prelude hiding (curry, uncurry)

{-------------------------------------------------------------------------------

CS:3820 Fall 2024 Problem Set 1
===============================

This problem covers the basics of Haskell syntax.  Each problem asks you to
write a function given its description, type signature, and several test cases.
You should change the structure of the equations as you want: adding new
equations, parameters, guards, &c.  You should consider the type signature part
of the problem specification, so you should not change it.

-------------------------------------------------------------------------------}

{-------------------------------------------------------------------------------

Part 1: Basic function definition
---------------------------------

In these problems, you should feel free to use any of the arithmetic operators
(+, -, *), logical operators (not, &&, ||), or comparison operators (==, /=, <,
&c.)    Haskell distinguishes between integer and fractional division: integer
division is written using the operators div and mod, whereas fractional division
is written with /.  

Remember that you can write either 

    div 4 2

or 

    4 `div` 2

The `` make a "normal" symbol into an operator.

-------------------------------------------------------------------------------}


-- 1.  Write a function that cubes its argument

cube :: Int -> Int
cube x = x * x * x

-- >>> cube 2
-- 8

-- >>> cube (-2)
-- -8

-- 2. Write a two-argument function that computes the difference of its cubed
--    arguments.  Recall that multiple arguments are written by "just" adding
--    more arguments: "f x y" is function "f" applied to arguments "x" and "y"

cubeDiff :: Int -> Int -> Int
cubeDiff x y = cube x - cube y

-- >>> cubeDiff 4 3
-- 37

-- >>> cubeDiff 2 (-2)
-- 16

-- 3. Write a function that evaluates to true if its argument is divisible by 7,
--    and false otherwise.  Recall that equations can be *guarded*: the
--    following function evaluates to M if P is true, and N otherwise:
--
--       f x | P         = M
--           | otherwise = N

seven :: Int -> Bool
seven x = x `mod` 7 == 0

-- >>> seven 2
-- False

-- >>> seven 56
-- True

-- >>> seven 0
-- True

-- 4. Write a two argument function that evalutes to 1 if its second argument is
--    divisible by its first argument, and 0 otherwise.
step :: Int -> Int -> Int
step x y | y `mod` x == 0 = 1
         | otherwise      = 0

-- >>> step 4 5
-- 0

-- >>> step 4 64
-- 1

{-------------------------------------------------------------------------------

Part 2: Pairs
-------------

Another of Haskell's built in types is the type of *pairs*.  Pairs---both types
and expressions---are written with (,).  For example, the type "(Int, Bool)"
denotes pairs of integers and Booleans; "(4, True)" is a value of this type.

When writing functions that use pairs, you have two choices.  There are
functions "fst" and "snd" that return the first and second components of a pair;
for example:

>>> fst (True, 1)
True

>>> snd (True, 1)
1

However, this is not a very "Haskell-y" way to do it.  Alternative, you can also
use (,) on the left-hand sides of your equations to take pair arguments apart.
This is an instance of a general idea called *pattern matching*.  For example,
we could define our own versions of the "fst" and "snd" functions by:

    myFst (x, y) = x
    mySnd (x, y) = y

In the following questions, you can use either "fst" and "snd" or pattern
matching.  However, I *strongly* recommend that you start practicing with
pattern matching now; it's an essential feature of Haskell, and the sooner
you're comfortable with it the better your semester is likely to go.    

-------------------------------------------------------------------------------}

-- 5. Write a function which computes the difference of a pair of integers.
pairDiff :: (Int, Int) -> Int
pairDiff (x, y) = x - y

-- >>> pairDiff (1, 4)
-- -3

-- >>> pairDiff (2, -4)
-- 6

-- 6. Write a two-argument function which, given a pair of integers (a, b) as
--    its first argument and a single integers x as its second, computes ax + b.
affine :: (Int, Int) -> Int -> Int
affine (a, b) x = a * x + b

-- >>> affine (2, 4) 3
-- 10

-- >>> affine (2, 5) (-3)
-- -1

-- 7. Write a function on integer pairs which:
--
--      (a) is *not* the identity function: for example, applying your function
--      to (1,2) should produce a result other than (1,2)
--
--      (b) is *idempotent*: that is to say, applying the function more than
--      once does the same thing as applying it once.

idem :: (Int, Int) -> (Int, Int)
idem (x, y) = (abs x, abs y)

-- 8. Write a function on pairs which:
--
--     (a) is not the identity function: for example, applying your function to
--     (1, 2) should produce a result other than (1, 2).
--
--     (b) is self-inverse: for example, applying your function to (1, 2)
--     *twice* should produce (1, 2)
--
--     (c) works for *arbitrarily* type values; that is to say, it works equally
--     well on pairs (Int,Int), pairs (Bool,Bool), and so forth.  In Haskell, we
--     write arbitrary types using *type variables*---the "a" in the type below
--     can be filled in by any type we want.


selfInv :: (a, a) -> (a, a)
selfInv (x, y) = (y, x)

-- Generic types, like the type of selfInv above, can actually be quite strong
-- specifications.  There are, of course, many many functions of type "(Int,
-- Int) -> (Int, Int)".  But how many functions of type "(a, a) -> (a, a)" can
-- you think of?

{-------------------------------------------------------------------------------

Part 3: Higher-order functions
------------------------------

A "higher-order" function is a function that takes other functions as arguments,
or produces other functions as results.  Of course, because Haskell is a
functional programming language, all functions in Haskell can be higher order;
there's no special syntax or different naming convention for higher-order
functions.

For example, the compose function behaves like the composition operator from
high school algebra:

    compose :: (b -> c) -> (a -> b) -> a -> c
    compose f g x = f (g x)

Recall that in Haskell, the composition operator is spelled ".": so "f . g" is
"compose f g".

-------------------------------------------------------------------------------}

-- 9. The order of arguments to composition is not always intuitive---if we
--    think about the order in which we do things, "f . g" means "do f after
--    doing g".  Write a function that combines functions in the opposite order.
before :: (a -> b) -> (b -> c) -> a -> c
before f g x = g (f x)

-- >>> (cube `before` (+1)) 4
-- 65

-- >>> ((+1) `before` cube) 4
-- 125

-- 10. Write a two argument function which, given a function "f" and a pair "p", returns
--     the same pair but with "f" applied to the second component of "p"
applySnd :: (b -> c) -> (a, b) -> (a, c)
applySnd f (x, y) = (x, f y)

-- >>> applySnd cube (1, 2)
-- (1,8)

-- >>> applySnd not (2, True)
-- (2,False)

-- It may occur to you that we now have two ways we could handle passing
-- multiple arguments to functions---either the way we have been doing it "f x
-- y", or by using pairs of arguments "f (x, y)".  The next two problems show
-- that these are equivalent, by giving functions that transform between the two
-- views.

-- 11. Write a function which, given a function that expects a pair as its first
--     argument, instead behaves like an equivalent function of two arguments. 
curry :: ((a, b) -> c) -> a -> b -> c
curry f x y = f (x, y)

-- >>> curry (\(x, y) -> x + y) 2 3
-- 5

-- >>> curry (\(b, c) -> b && c) True False
-- False

-- 12. Write a function which, given a function that expects two arguments,
--     instead behaves like an equivalent function that expects a pair as its
--     argument.
uncurry :: (a -> b -> c) -> (a, b) -> c
uncurry f (x, y) = f x y

-- >>> uncurry (+) (2,3)
-- 5

-- >>> uncurry (&&) (True, False)
-- False

{-------------------------------------------------------------------------------

Part 4: Lists
-------------

The final built-in type we're going to consider is lists.  Lists in Haskell are
written with []: [Int] is the type of lists of ints, and [1,2,3] is a value of
that type.

Normally, to write functions over lists, we'd need at least one more language
feature than we've seen so far: something like iteration, or looping, or
recursion.  But with higher-order functions in Haskell, we can actually describe
a fair number of list computations without needing to resort to those kinds of
features.  Here are some of Haskell's list-manipulating functions:

  - map :: (a -> b) -> [a] -> [b]

    "map f xs" computes the result of applying function "f" to each element in
    "xs".  For example:

>>> map not [True, False]
[False,True]

  - filter :: (a -> Bool) -> [a] -> [a]

    "filter f xs" returns a list consisting of those elements of "xs" for which
    "f" computes True.  For example:

>>> filter (0 ==) [0,1,2,3]
[0]

  - sum :: [Int] -> Int
  - length :: [a] -> Int

    These functions are more self-explanatory: "sum xs" computes the sum of the
    values in "xs", whereas "length xs" computes the number of elements in "xs".

-------------------------------------------------------------------------------}

-- 13. Write a function which, given a list of integers, returns a list in which
--     each of the integers has been cubed.
--
--     Note: this probably originally (incorrectly) said that the resulting list
--     should be *doubled*, not *cubed*.  In the interests of fairness, we will
--     accept solutions in which the `cubed` function *either* doubles *or*
--     cubes its argument.
cubed :: [Int] -> [Int]
cubed = map cube

-- >>> cubed [1,2,3]
-- [1,8,27]

-- 14. Write a function which, given a list of integers "xs", returns a list of
--     the values in "xs" divisible by 7
sevens :: [Int] -> [Int]
sevens = filter seven

-- >>> sevens [0..50]
-- [0,7,14,21,28,35,42,49]

-- 15. Write a function which, given a list of integers "xs", returns the
--     *number* of even integers in "xs".
countEven :: [Int] -> Int
countEven = sum . map (step 2)

-- >>> countEven [0..49]
-- 25

-- 16. Write a function which computes the average of a list of integers.
--     Remember that Haskell distinguishes between integer and floating point
--     division---you can't just apply the floating point division operator / to
--     two integers.  To convert integers to doubles (or indeed to any of many
--     other types) use the function "fromIntegral".
average :: [Int] -> Double
average xs = fromIntegral (sum xs) / fromIntegral (length xs)

-- >>> average [1..4]
-- 2.5

-- >>> average [0..4]
-- 2.0
