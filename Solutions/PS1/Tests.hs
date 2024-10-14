module Tests where

-- GHC
import System.Exit
import System.Environment

import Prelude hiding ( even, curry, uncurry)

-- External
import Test.HUnit

-- Lib
import Problems1
import GHC.Base (undefined)
import Text.Read (Lexeme(String))

err :: String -> String
err problem = ""
--------------------------------------------------------------------------------
-- Part 1
--------------------------------------------------------------------------------


tests1, tests2, tests3, tests4 :: Test
tests1 = test [
  -- double
  cube 0           @?= 0,
  cube 1           @?= 1,
  cube 2           @?= 8,
  cube 12345       @?= 1881365963625,
  cube (-1337)     @?= -2389979753
  ]

tests2 = test [
  -- doubleProd
  cubeDiff 0 1     @?= -1,
  cubeDiff 1 2     @?= -7,
  cubeDiff 4 3     @?= 37,
  cubeDiff 40 20   @?= 56000,
  cubeDiff (-7) 13 @?= -2540
  ]

tests3 = test [
  -- even
  seven 49            @?= True,
  seven 1             @?= False,
  seven (7^10 :: Int) @?= True,
  seven (3^12 :: Int) @?= False
  ]

tests4 = test [
  -- step
  step 2 10          @?= 1,
  step 12 25         @?= 0,
  step 1 0           @?= 1,
  step (-21) 600     @?= 0
  ]

--------------------------------------------------------------------------------
-- Part 2
--------------------------------------------------------------------------------
tests5, tests6, tests7, tests8 :: Test
tests5 = test [
  -- pairSum
  pairDiff (1, 2) @?= -1,
  pairDiff (4, 3) @?= 1,
  pairDiff (pairDiff (pairDiff (pairDiff (0, 1000), 200), 30), 4)
      @?= -1234
  ]

tests6 = test [
  -- affine
  affine (1, 2) 3 @?= 5,
  affine (3, 4) 1 @?= 3 + 4,
  affine (10, 1) 40 @?= 401
  ]


assertUnequal :: (Show a, Eq a) => a -> a -> Assertion
assertUnequal expected actual = assertBool ("expected " ++ show actual ++ " to differ from " ++ show actual) (expected /= actual)

tests7 =  test $
  -- selfInv
  assertBool "expected at least one unequal pair from (-15,-14) to (15, 16)" (or [(x, y) /= idem (x, y) | x <- [-15..15], let y = x + 1]) : 
  [ (idem . idem $ (x, y)) @?= idem (x, y) | x <- [1..15], let y = x + 1]

tests8 = test $
  -- selfInv
  [ (selfInv . selfInv $ (x, y)) @?= (x, y) | x <- [1..15], let y = x + 1]
  ++
  [ assertUnequal (x, y) (selfInv (x, y)) | x <- [1..15], let y = x + 1 ]
  ++
  [ (selfInv . selfInv $ (x, y)) @?= (x, y) | x <- [True,False], y <- [True, False]]
  ++
  [ assertUnequal (x, y) (selfInv (x, y)) | x <- [True, False], y <- [True, False], x /= y]

--------------------------------------------------------------------------------
-- Part 3
--------------------------------------------------------------------------------
tests9, tests10, tests11, tests12 :: Test

-- before
tests9 = test
  [ before show length 1337                    @?= 4
  , before (* 2) (before (`mod` 2) (== 0)) 43    @?= True
  , before (const "Hi there") (const "Bye!") 0 @?= "Bye!"
  ]

-- applyFst
tests10 = test
  [ applySnd show (True,0)                  @?= (True, "0")
  , applySnd fst ('H', ("askell", [True]))  @?= ('H', "askell")
  , applySnd (map show) ([1,2,3], [4,5,6])  @?= ([1,2,3], ["4", "5", "6"])
  ]

-- curry
tests11 = test
  [ curry fst True False                   @?= True
  , curry (\ x -> fst x + snd x) 1.1 2.2   @?= 1.1 + 2.2
  , curry (\ x -> (snd x , fst x)) 'a' 'c' @?= ('c', 'a')
  ]

tests12 = test
  [ uncurry const (True, False)   @?= True
  , uncurry (+) (1.1, 2.2)        @?= 1.1 + 2.2
  , uncurry (flip (,)) ('a', 'c') @?= ('c', 'a')
  , (curry . uncurry) (*) 3 7     @?= 21
  ]

--------------------------------------------------------------------------------
-- Part 4
--------------------------------------------------------------------------------
tests13, tests14, tests15, tests16 :: Test

-- doubled
tests13 = test
  [ cubed []                 @?= []
  , assertBool "Expect `cubed [1..10] to be `[1,8,27,64,125,216,343,512,729,1000]`"
              (cubed [1..10] == [1,8,27,64,125,216,343,512,729,1000] ||
               cubed [1..10] == [2,4,6,8,10,12,14,16,18,20]) 
  , assertBool "Expect `cubed [1337, 525600, 23]` to be `[2389979753,145199817216000000,12167]`"
               (cubed [1337, 525600, 23] == [2389979753,145199817216000000,12167] ||
                cubed [1337, 525600, 23] == [2674,1051200,46])
  , assertBool "Expected `cubed [-25..-1]` to be `[-15625,-13824,-12167,-10648,-9261,-8000,-6859,-5832,-4913,-4096,-3375,-2744,-2197,-1728,-1331,-1000,-729,-512,-343,-216,-125,-64,-27,-8,-1]`"
               (cubed [-25 .. -1] == [-15625,-13824,-12167,-10648,-9261,-8000,-6859,-5832,-4913,-4096,-3375,-2744,-2197,-1728,-1331,-1000,-729,-512,-343,-216,-125,-64,-27,-8,-1] ||
                cubed [-25 .. -1] == [-50,-48,-46,-44,-42,-40,-38,-36,-34,-32,-30,-28,-26,-24,-22,-20,-18,-16,-14,-12,-10,-8,-6,-4,-2])
  ]

-- evens
tests14 = test
  [ sevens [1..100]    @?= [7,14,21,28,35,42,49,56,63,70,77,84,91,98]
  , sevens []          @?= []
  , sevens [-25 .. -1] @?= [-21,-14,-7]
  , sevens [1]         @?= []
  ]

-- countPositive
tests15 = test
  [ countEven []                   @?= 0
  , countEven [0, -1 .. -20]       @?= 11
  , countEven [-25..25]            @?= 25
  , countEven [-3820, 3802, -3820] @?= 3
  ]

-- average
tests16 = test
  [ average [] == average []          @?= False -- NaN != NaN
  , average [1]                       @?= 1
  , average [-2, 0, 11]               @?= 3
  , average [13, 17, 19, 23, 29]      @?= 20.2
  , average [-31, -37, -41, -43, -47] @?= -39.8
  ]

--------------------------------------------------------------------------------
-- main
--------------------------------------------------------------------------------

allTests :: [Test]
allTests = [ tests1, tests2, tests3, tests4
           , tests5, tests6, tests7, tests8
           , tests9, tests10, tests11, tests12
           , tests13, tests14, tests15, tests16 ]

argMap :: Int -> Test
argMap n
  | n > 0 && n <= 16 = allTests !! (n - 1)
  | otherwise        = test allTests

hd :: [a] -> Maybe a
hd (x : _) = Just x
hd []       = Nothing

main :: IO ()
main = do
  args <- getArgs
  let tests = case read <$> (hd args) of
                Just x -> argMap x
                Nothing -> argMap 42
  results <- runTestTT tests
  if (errors results + failures results == 0)
    then
      exitWith ExitSuccess
    else
      exitWith (ExitFailure 1)
