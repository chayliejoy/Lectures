{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant lambda" "Use const" #-}
module Lecture9 (module Lecture9) where

data Expr = Const Int | Plus Expr Expr | Var String | Lam String Expr | App Expr Expr
          | Store Expr | Recall
  deriving (Eq, Show)

(@) :: Expr -> Expr -> Expr
(@) = App
infixl 9 @

e1 = Plus (Store (Const 2)) Recall
-- all of you expect 4
-- I expect 2

-- Come back here: can we force the order of evaluation?

e2 = Lam "x" (Plus Recall (Var "x")) @ Store (Const 1)
-- y'all expect 2
-- I expect 1

e2' = Lam "x" (Plus Recall (Const 1)) @ Store (Const 1)

e3 = Lam "x" (Plus Recall (Var "x")) @ Store (Plus (Const 1) Recall)

e4 = Store (Lam "x" (Recall @ Var "x")) @ Const 1

omega = Lam "x" (Var "x" @ Var "x") @ Lam "x" (Var "x" @ Var "x")

-- let's extend our language with `let`

let_ :: String -> Expr -> Expr -> Expr
let_ x m n = Lam x n @ m    

-- let_ "x" (Const 1) (Plus (Var "x") (Var "x"))  ~~>  (Lam "x" (Plus (Var "x") (Var "x")) @ Const 1)

e1' = let_ "x" (Store (Const 2)) $
      let_ "y" Recall $ 
      Plus (Var "y") (Var "x")
      
e1'' = let_ "y" Recall $ 
       let_ "x" (Store (Const 2)) $
       Plus (Var "y") (Var "x")
isValue :: Expr -> Bool
isValue (Const _) = True
isValue (Lam _ _) = True
isValue _ = False

substitute :: String -> Expr -> Expr -> Expr
substitute _ _ (Const i) = Const i
substitute x m (Plus n1 n2) = Plus (substitute x m n1) (substitute x m n2)
substitute x m (Var y)
  | x == y = m
  | otherwise = Var y
substitute x m (Lam y n)
  | x == y = Lam y n
  | otherwise = Lam y (substitute x m n)
substitute x m (App n1 n2) = App (substitute x m n1) (substitute x m n2)
substitute x m (Store n) = Store (substitute x m n)
substitute x m Recall = Recall

-- Note: first component is *expression*, second component is *state*
smallStep :: Bool -> Bool -> (Expr, Expr) -> Maybe (Expr, Expr)
smallStep ltr cbv (Const _, _) = Nothing
smallStep ltr cbv (Plus (Const i) (Const j), s) = Just (Const (i + j), s)
smallStep ltr cbv (Plus m n, s) 
  | ltr = case (smallStep ltr cbv (m, s), smallStep ltr cbv (n, s)) of
            (Just (m', s'), _) -> Just (Plus m' n, s')
            (_, Just (n', s')) -> Just (Plus m n', s')
            _                  -> Nothing
  | otherwise = case (smallStep ltr cbv (m, s), smallStep ltr cbv (n, s)) of
                  (_, Just (n', s')) -> Just (Plus m n', s')
                  (Just (m', s'), _) -> Just (Plus m' n, s')
                  _                  -> Nothing
smallStep ltr cbv (Var _, _) = Nothing
smallStep ltr cbv (Lam _ _, _) = Nothing
smallStep ltr cbv (App (Lam x m) n, s) 
  | isValue n || not cbv = Just (substitute x n m, s)
smallStep ltr cbv (App m n, s) 
  | not cbv = case smallStep ltr cbv (m, s) of
                Just (m', s') -> Just (App m' n, s')
                Nothing       -> Nothing
  | ltr = case (smallStep ltr cbv (m, s), smallStep ltr cbv (n, s)) of
            (Just (m', s'), _) -> Just (App m' n, s')
            (_, Just (n', s')) -> Just (App m n', s')
            _                  -> Nothing
  | otherwise = case (smallStep ltr cbv (m, s), smallStep ltr cbv (n, s)) of
                  (_, Just (n', s')) -> Just (App m n', s')
                  (Just (m', s'), _) -> Just (App m' n, s')
                  _                  -> Nothing
smallStep ltr cbv (Store m, s) 
  | isValue m || not cbv = Just (m, m)
  | otherwise = case smallStep ltr cbv (m, s) of
                  Just (m', s') -> Just (Store m', s')
                  Nothing       -> Nothing
smallStep ltr cbv (Recall, s)  = Just (s, s)

steps :: Bool -> Bool -> (Expr, Expr) -> [(Expr, Expr)]
steps ltr cbv m = case smallStep ltr cbv m of
            Nothing -> [m]
            Just m' -> m : steps ltr cbv m'

prints :: Show a => [a] -> IO ()
prints = mapM_ print

bigStep :: Bool -> Bool -> Expr -> Expr -> Maybe (Expr, Expr)
bigStep ltr cbv (Const i) acc = Just (Const i, acc)
bigStep ltr cbv (Plus m1 m2) acc =
  case bigStep ltr cbv n1 acc of
    Just (Const i, acc') -> 
      case bigStep ltr cbv n2 acc' of
        Just (Const j, acc'') -> Just (Const (i + j), acc')
        _ -> Nothing
    _ -> Nothing
  where (n1, n2) | ltr = (m1, m2)
                 | otherwise = (m2, m1)
bigStep _ _ (Var _) _ = Nothing
bigStep _ _ (Lam x m) acc = Just (Lam x m, acc)
bigStep ltr cbv (App m n) acc
  | not cbv =
    case bigStep ltr cbv m acc of
      Just (Lam x m, acc') -> bigStep ltr cbv (substitute x n m) acc'
      _ -> Nothing
  | ltr =
    case bigStep ltr cbv m acc of
      Just (Lam x m, acc') -> 
        case bigStep ltr cbv n acc' of
          Just (v, acc'') -> bigStep ltr cbv (substitute x n m) acc''
          _ -> Nothing
      _ -> Nothing
  | otherwise = 
    case bigStep ltr cbv n acc of
      Just (v, acc') -> 
        case bigStep ltr cbv m acc' of
          Just (Lam x m, acc'') -> bigStep ltr cbv (substitute x n m) acc''
          _ -> Nothing
      _ -> Nothing
bigStep ltr cbv (Store m) acc =
  case bigStep ltr cbv m acc of
    Just (v, acc') 
      | cbv -> Just (v, v)
      | otherwise -> Just (v, m)
    _ -> Nothing
bigStep ltr cbv Recall acc =
  Just (acc, acc)

-- But there's a lot of repetition there.  Let's see if we can do better.  Our
-- first observation is that the `case`s over `Maybe` results of evaluation
-- always have the same pattern.  We can abstract like this:

tryM :: Maybe a -> (a -> Maybe b) -> Maybe b
tryM Nothing k = Nothing
tryM (Just x) k = k x

-- in `tryM m k`, `m` represents a computation that may have failed; `k`
-- represents the remainder (or continuation) of the computation to run in case
-- `m` succeeded.  

-- Just to be sanitary, we'll wrap the remaining uses of the `Maybe`
-- constructors in abstractions as well.. perhaps that will pay off in 20 lines
-- or so.

doneM :: a -> Maybe a
doneM x = Just x

errM :: Maybe a
errM = Nothing

-- We can also separate the checks on the type of value we get back from
-- evaluation; note that while the types refer to `Maybe`, we can write the
-- functions themselves purely in terms of our `done` and `err` abstractions.

intValM :: Expr -> Maybe Int
intValM (Const i) = doneM i
intValM _         = errM

funValM :: Expr -> Maybe (String, Expr)
funValM (Lam x m) = doneM (x, m)
funValM _         = errM

-- Here's the evaluator, without a single `case` to found!

bigStep1 :: Bool -> Bool -> Expr -> Expr -> Maybe (Expr, Expr)
bigStep1 ltr cbv (Const i) acc = doneM (Const i, acc)
bigStep1 ltr cbv (Plus m1 m2) acc =
  tryM (bigStep1 ltr cbv n1 acc) $ \ (v, acc') ->
  tryM (intValM v) $ \ i ->
  tryM (bigStep1 ltr cbv n2 acc') $ \ (w, acc'') ->
  tryM (intValM w) $ \ j ->
    doneM (Const (i + j), acc'')
  where (n1, n2) | ltr = (m1, m2)
                 | otherwise = (m2, m1)
bigStep1 _ _ (Var _) acc = errM
bigStep1 _ _ (Lam x m) acc = doneM (Lam x m, acc)
bigStep1 ltr cbv (App m n) acc
  | not cbv =
    tryM (bigStep1 ltr cbv m acc) $ \ (v, acc') ->
    tryM (funValM v) $ \ (x, m) ->
      bigStep1 ltr cbv (substitute x n m) acc'
  | ltr = 
    tryM (bigStep1 ltr cbv m acc) $ \ (v, acc') ->
    tryM (funValM v) $ \ (x, m) ->
    tryM (bigStep1 ltr cbv n acc') $ \ (w, acc'') ->
      bigStep1 ltr cbv (substitute x n m) acc''
  | otherwise = 
    tryM (bigStep1 ltr cbv n acc) $ \ (w, acc') ->
    tryM (bigStep1 ltr cbv m acc') $ \ (v, acc'') ->
    tryM (funValM v) $ \ (x, m) -> 
      bigStep1 ltr cbv (substitute x n m) acc''
bigStep1 ltr cbv (Store m) acc =
  tryM (bigStep1 ltr cbv m acc) $ \ (v, acc') ->
  if cbv then Just (v, v) else Just (v, m)
bigStep1 ltr cbv Recall acc = 
  Just (acc, acc)

-- That's not all the repeating ourselves tho.  We also do a lot of manipulation
-- of the accumulator, even though it only "matters" in implementing `Store` and
-- `Recall`.  Let's try to abstract that as well, using the same patterns.

tryMS :: (s -> Maybe (a, s)) -> (a -> (s -> Maybe (b, s))) -> (s -> Maybe (b, s))
tryMS m k = \ acc ->
  tryM (m acc) (uncurry k)

-- The key new idea is that instead of potentially-failing values, we manipulate
-- potentially-failing state *transformers*.  So, in `tryMS m k`, `m` is a
-- *state-transformer* that may fail; `k` is the continuation in case `m`
-- succeeeds.  Note that we're able to build this version by layering the
-- accumulator logic on top of our existing `try` function for `Maybe`.

-- New versions of `done` and `err` can also be layered on top of the previous
-- versions

doneMS :: a -> (s -> Maybe (a, s))
doneMS x = \ acc -> doneM (x, acc)

errMS :: s -> Maybe (a, s)
errMS = \ acc -> errM

-- We have to update the result types of `intVal` and `funVal`, but their bodies
-- remain the same.  (Hmm, this suggests we need a better type-level description
-- of this abstraction.)

intValMS :: Expr -> s -> Maybe (Int, s)
intValMS (Const i) = doneMS i
intValMS _         = errMS

funValMS :: Expr -> s -> Maybe ((String, Expr), s)
funValMS (Lam x m) = doneMS (x, m)
funValMS _         = errMS

-- Now, big-step evaluation without either `case`s or excessive `acc`
-- manipulation.

bigStep2 :: Bool -> Bool -> Expr -> Expr -> Maybe (Expr, Expr)
bigStep2 ltr cbv (Const i) = doneMS (Const i)
bigStep2 ltr cbv (Plus m1 m2) =
  tryMS (bigStep2 ltr cbv n1) $ \ v ->
  tryMS (intValMS v) $ \ i ->
  tryMS (bigStep2 ltr cbv n2) $ \ w ->
  tryMS (intValMS w) $ \ j ->
    doneMS (Const (i + j))
  where (n1, n2) | ltr = (m1, m2)
                 | otherwise = (m2, m1)
bigStep2 _ _ (Var _) = errMS
bigStep2 _ _ (Lam x m) = doneMS (Lam x m)
bigStep2 ltr cbv (App m n)
  | not cbv =
    tryMS (bigStep2 ltr cbv m) $ \ v ->
    tryMS (funValMS v) $ \ (x, m) ->
      bigStep2 ltr cbv (substitute x n m)
  | ltr = 
    tryMS (bigStep2 ltr cbv m) $ \ v ->
    tryMS (funValMS v) $ \ (x, m) ->
    tryMS (bigStep2 ltr cbv n) $ \ w ->
      bigStep2 ltr cbv (substitute x n m)
  | otherwise = 
    tryMS (bigStep2 ltr cbv n) $ \ w ->
    tryMS (bigStep2 ltr cbv m) $ \ v ->
    tryMS (funValMS v) $ \ (x, m) -> 
      bigStep2 ltr cbv (substitute x n m)
-- Arguably, we could have introduced functions for getting and setting the
-- accumulator, like `errMS` wraps the `Nothing`.  However, we can also open our
-- abstraction, only when we need it.  Note that we can still use `tryMS` to
-- handle the evaluation of `m` in `Store m`, and only open the abstraction
-- afterwards.
bigStep2 ltr cbv (Store m) =
  tryMS (bigStep2 ltr cbv m) $ \ v acc ->
  if cbv then Just (v, v) else Just (v, m)
bigStep2 ltr cbv Recall = \ acc ->
  Just (acc, acc)

main :: IO ()
main = return ()
