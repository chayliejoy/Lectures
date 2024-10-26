{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Avoid lambda using `infix`" #-}
module Lecture7 (module Lecture7) where

{-------------------------------------------------------------------------------

Lecture 7
=========

Our goal is to explore big-step and small-step semantics by realizing them in
Haskell.  We need a language to study, here including both arithmetic and pair
operations.

-------------------------------------------------------------------------------}

data Expr = Const Int | Plus Expr Expr | Times Expr Expr | Negate Expr
          | Pair Expr Expr | Fst Expr | Snd Expr
  deriving Show

-- Toy examples, like this one, always have some goals in mind; here, I wanted
-- to have both binary and unary operations, and two types.  Of course, the
-- `Negate` operation isn't *actually* necessary; we could just multiply by -1.

{-------------------------------------------------------------------------------

Reduction ends when we reach an expression in *simplest* form; that is to say,
when we reach an expression that doesn't have any more computation we could do.
We can write a Haskell function that identifies such expressions:

-------------------------------------------------------------------------------}

isValue :: Expr -> Bool
isValue (Const _) = True
isValue (Pair m n) = isValue m && isValue n
    -- This case is a little subtle: unlike constants, not *all* pairs are
    -- values; instead, pairs *of values* are values.  The pair `Pair (Plus 1 2)
    -- (Const 3)`, on the other hand, could still reduce further.
isValue _ = False

{-------------------------------------------------------------------------------

Big-step semantics
------------------

First, we'll write a big-step semantics for expressions.  Remember that a
big-step semantics reaches the simplest form in a single ("big") step.  We'll
start out by just writing this as an `Expr -> Expr` function, but in which we
want to maintain the invariant that we always produce a value.

-------------------------------------------------------------------------------}

bigStep :: Expr -> Expr
bigStep (Const i) = Const i
  -- Constants are already in simplest form
bigStep (Plus m n) = Const (asConst (bigStep m) + asConst (bigStep n))
  -- If we're going to add `m` and `n`, we need to know:
  --   * The constant to which `m` reduces (call it i)
  --   * The constant to which `n` reduces (call it j)  
  -- and then we can produce the constant `i + j`.  The helper function
  -- `asConst` picks the constant out of the recursive invocations of `bigStep`
bigStep (Times m n) = Const (asConst (bigStep m) * asConst (bigStep n))  
bigStep (Negate m) = Const (negate (asConst (bigStep m)))
  -- Similar reasoning to `Plus`
bigStep (Pair m n) = Pair (bigStep m) (bigStep n)  
  -- Pairs may not be in simplest form, but we don't have any requirements on
  -- the kinds of expressions to which the components reduce.`
bigStep (Fst m) = fst (asPair (bigStep m))
  -- To get the first component from `m`, we need to know to which pair `m`
  -- reduces.
bigStep (Snd m) = snd (asPair (bigStep m))


-- Helper functions

asConst :: Expr -> Int
asConst (Const i) = i
asConst e = error ("Expected " ++ show e ++ " to be an integer constant")

asPair :: Expr -> (Expr, Expr)
asPair (Pair m n) = (m, n)
asPair e = error ("Expected " ++ show e ++ " to be a pair constant")
