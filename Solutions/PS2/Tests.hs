{-# LANGUAGE StandaloneDeriving #-}
module Tests where

import Data.List (partition, tails)
import System.Exit ( exitWith, ExitCode(ExitFailure, ExitSuccess) )
import System.Environment ( getArgs )
import Test.HUnit
import Text.Read (readMaybe)

import Problems2

deriving instance Eq K3

tests1, tests2 :: Test

tests1 = test [
    and3 T T @?= T
  , and3 T I @?= I
  , and3 I F @?= F
  , and3 F F @?= F
  ]

tests2 = test [
    or3 T T @?= T
  , or3 T I @?= T
  , or3 I I @?= I
  , or3 F I @?= I
  , or3 F F @?= F
  ]

deriving instance Eq B4

tests3, tests4 :: Test

tests3 = test [
    and4 b b @?= b
  , and4 b t @?= b
  , and4 t t @?= t
  , and4 t f @?= f
  , and4 n f @?= f
  , and4 t n @?= n
  ]

tests4 = test [
    or4 b b @?= b
  , or4 b t @?= t
  , or4 b f @?= b
  , or4 t f @?= t
  , or4 n t @?= t
  , or4 n f @?= n
  ]

tests5, tests6 :: Test

tests5 = test [
    same Spades Spades @?= True
  , same Spades Hearts @?= False
  , same Hearts Diamonds @?= False
  , same Diamonds Diamonds @?= True
  , same Clubs Spades @?= False
  , same Clubs Clubs @?= True
  ]

tests6 = test [
    leaderTakes Spades (Card 1 Spades) (Card 10 Spades) @?= True
  , leaderTakes Spades (Card 4 Hearts) (Card 2 Spades) @?= False
  , leaderTakes Spades (Card 4 Hearts) (Card 2 Clubs) @?= True
  , leaderTakes Hearts (Card 10 Clubs) (Card 9 Clubs) @?= True
  , leaderTakes Diamonds (Card 10 Clubs) (Card 2 Diamonds) @?= False
  ]

tests7, tests8, tests9 :: Test

tests7 = test [
    byDefault 'c' (Just 'd') @?= 'd'
  , byDefault 'c' Nothing @?= 'c'
  ]

tests8 = test [
    addMissing (Just 4) (Just 10) @?= Just 14
  , addMissing (Just 10) Nothing @?= Just 10
  , addMissing Nothing Nothing @?= Nothing
  , addMissing Nothing (Just 21) @?= Just 21 
  ]

tests9 = test [
    addErroneous Nothing (Just 4) @?= Nothing
  , addErroneous (Just 10) (Just 14) @?= Just 24
  , addErroneous Nothing Nothing @?= Nothing
  , addErroneous (Just 13) Nothing @?= Nothing
  ]    

tests10, tests11 :: Test

tests10 = test $ concat [[(bigger s s @?= False) : [bigger s s' @?= True | s' <- ss]] | (s : ss) <- tails [Spades, Hearts, Diamonds, Clubs]]

tests11 = test [
    outbids (Bid 4 (Just Spades)) (Bid 4 (Just Hearts)) @?= True
  , outbids (Bid 4 (Just Diamonds)) (Bid 4 (Just Hearts)) @?= False
  , outbids (Bid 4 (Just Spades)) (Bid 3 Nothing) @?= True
  , outbids (Bid 4 Nothing) (Bid 4 (Just Spades)) @?= True
  , outbids (Bid 3 Nothing) (Bid 4 (Just Clubs)) @?= False
  ]

--------------------------------------------------------------------------------
-- main
--------------------------------------------------------------------------------

allTests :: [(Int, Test)]
allTests = zip [1..] [ tests1, tests2, tests3, tests4, tests5, tests6, tests7
                     , tests8, tests9, tests10, tests11 ]

main :: IO ()
main = do
  allArgs <- getArgs
  let (makeJson, testIdxs) = partition ("json" ==) allArgs
      tests | null testIdxs = allTests
            | otherwise = pickTests (ranges testIdxs)
  
  if not (null makeJson) 
  then putStrLn (testsJson tests)
  else do results <- runTestTT (test $ map snd tests)
          if (errors results + failures results == 0)
            then
              exitWith ExitSuccess
            else
              exitWith (ExitFailure 1)

  where ranges [] = []
        ranges (s : ss)
            | Just i <- readMaybe s          = i : ranges ss
            | otherwise                      = error "Unknown ranges"
            where go []           = []
                  go (i : j : ks) = i : j : go ks
      
        pickTests (i : j : ks) = between i j ++ pickTests ks
        pickTests [i]          = [allTests !! (i - 1)]
        pickTests []           = []
      
        between i j = take (j - i + 1) (drop (i - 1) allTests)

        testsJson tests =
            unlines [ "{\"name\" : \"Problem " ++ show n ++ "\", \"setup\" : \"\", \"run\" : \"cabal run Test " ++ show n ++ "\", \"input\" : \"\", \"output\" : \"\", \"comparison\" : \"included\", \"timeout\" : 0.5, \"points\" : 1 },"
                    | (n, t) <- tests
                    ]
