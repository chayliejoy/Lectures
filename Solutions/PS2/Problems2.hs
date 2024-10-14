module Problems2 where

{-------------------------------------------------------------------------------

CS:3820 Fall 2024 Problem Set 2
===============================

This problem explores programming with data types and pattern matching

-------------------------------------------------------------------------------}


{-------------------------------------------------------------------------------

Problems 1 & 2: Three-valued logic
----------------------------------

These problem considers (one approach to) three-valued logic.  In this approach,
in addition to the usual values `T` for truth and `F` for falsity, we have `I`
for the *indeterminate* value---that is to say, one that may be *either* true
*or* false.

-------------------------------------------------------------------------------}


data K3 = T | I | F
  deriving Show  -- this makes it possible to print `K3` values out

---------------------------------------------------------------------------------
-- Problem 1: and3
--
-- Your first problem is to implement 3-valued conjunction (that is, and).
-- Intuitively, `and3 b1 b2` starts from the normal 2-valued definition of
-- conjunction (i.e., `and3 T T` should be `T`, `and3 F F` should be `F`).  We
-- extend this definition to include indeterminate values by saying that `and3 b1
-- b2` should be:
--
--   * `T` if `b1 and b2` would be true regardless of whether indeterminate
--     values are true or false
--   * `F` if `b1 and b2` would be false regardless of whether indeterminate
--     values are true or false
--   * `I` if `b1 and b2` could be either true or false, depending on the
--     whether indeterminate values are true or false.
--
-- Note that we consider each indeterminate value *separately*; for example,
-- `and3 I I` could be `and3 T T`, `and3 T F`, and so on.

and3 :: K3 -> K3 -> K3
and3 T T = T 
and3 T I = I
and3 I T = I
and3 I I = I
and3 _ _ = F

--------------------------------------------------------------------------------
-- Problem 2: or3
--
-- Your second problem is to implement 3-valued disjunction.  This works the
-- same way as 3-valued conjunction: 
--
--  * on definite values, it's identical to the 2-valued disjunction
--  * on indefinite values, it's true (resp. false) if the disjunction *must* be
--    true (resp. false) regardless of whether indeterminate values are true or
--    false, and indeterminate otherwise.

or3 :: K3 -> K3 -> K3 
or3 F F = F
or3 F I = I
or3 I F = I
or3 I I = I
or3 _ _ = T 

{-------------------------------------------------------------------------------

Problems 3 & 4: Four-valued logic
---------------------------------

The previous problems considered a third logical state which could be either
true or false.   In these problems, we complete the picture by including a
fourth state which is neither true nor false.  These states are conventionally
named

    B     both true and false
    T     true
    F     false
    N     neither true nor false

We could follow the same pattern we did in the previous problems, and define an
enumeration of these states:

    data B4 = B | T | F | N

However, implementing the logical operations in terms of this enumeration would
be exhausting.  Instead, let's represent each state by *two* Boolean values, one
to represent its "truthiness" (that is, whether it can be true) and a second to
represent its "falsiness" (that is, whether it can be false).

-------------------------------------------------------------------------------}

data B4 = B4 Bool Bool

{-------------------------------------------------------------------------------

Now, we can interpret each of the states before as one of our `B4` values:

-------------------------------------------------------------------------------}

b, t, f, n :: B4
b = B4 True True
t = B4 True False
f = B4 False True
n = B4 False False

{-------------------------------------------------------------------------------

We can even convince Haskell to print out B4 values as if we'd defined them by
enumerating them.  (Note that this *only* determines how they're printed out,
*not* how you type them in, or pattern match on them, or anything like that.)

-------------------------------------------------------------------------------}

instance Show B4 where
  show (B4 False False) = "N"
  show (B4 False True)  = "F"
  show (B4 True False)  = "T"
  show (B4 True True)   = "B"

isTrue, isFalse :: B4 -> Bool
isTrue (B4 true false) = true
isFalse (B4 true false) = false

----------------------------------------------------------------------------------
-- Problems 3: and4
--
-- Your next problem is to implement the 4-valued version of conjunction.
-- Intuitively:
--
--   * `and4 b1 b2` should be truthy if `b1` and `b2` are both truthy.
--   * `and4 b1 b2` should be falsey if either `b1` or `b2` is falsey.
--
-- For example: `and4 b t` should be `B`, `and4 f n` should be `F`, and `and4 t
-- n` should be `N`.

and4 :: B4 -> B4 -> B4

and4 b1 b2 = B4 (isTrue b1 && isTrue b2) (isFalse b1 || isFalse b2)

--------------------------------------------------------------------------------
-- Problem 4: or4
--
-- Next, you should implement the 4-valued version of disjunction.  Intuitively:
--
--   * `or4 b1 b2` should be truthy if either `b1` or `b2` are truthy.
--   * `or4 b1 b2` should be falsey if both `b1` and `b2` are falsey
--
-- For example `or4 b t` should be `T`, `or4 b b` should be `B`, and `or4 n f`
-- should be `N`.


or4 :: B4 -> B4 -> B4

or4 b1 b2 = B4 (isTrue b1 || isTrue b2) (isFalse b1 && isFalse b2)

{-------------------------------------------------------------------------------

Problems 5--11: Cards
---------------------

The final problems implement some common operations in card games, as a way to
justify operations over a larger enumeration (of suits).

We'll start by defining the suits:

-------------------------------------------------------------------------------}

data Suit = Spades | Hearts | Diamonds | Clubs
  deriving Show

--------------------------------------------------------------------------------
-- Problem 5: same
--
-- For your next problem, write a function `same`, which returns `True` if its
-- two arguments are the same suit and `False` otherwise.  For example, `same
-- Spades Spades` should evaluate to `True`, but `same Spades Hearts` should
-- evaluate to `False`.

same :: Suit -> Suit -> Bool
same Spades Spades     = True
same Hearts Hearts     = True
same Diamonds Diamonds = True
same Clubs Clubs       = True
same _ _               = False

{-------------------------------------------------------------------------------

A card combines a rank (that is to say, ace, deuce, Jack, or whatever) and a
suit.  We'll represent ranks by integers, where 1 = Ace, 2 through 10 are the
corresponding numbers, and then Jack, Queen and King are 11 through 13.

-------------------------------------------------------------------------------}

data Card = Card Int Suit
  deriving Show

--------------------------------------------------------------------------------
-- Problem 6: leaderTakes
--
-- For this problem, you'll implement the logic standard to most trick-taking
-- games with trump. 
--
-- Your function is given three pieces of information: the trump suit, which
-- card the leader played, and which card the trailing player played.  Your
-- function should return `True` if the leading player wins the trick, and
-- `False` otherwise.
--
-- The trailing player wins if either: they play a higher card in the same suit
-- as the leading card, or they play a trump card (and the leading card was not
-- trump).
--
-- The leading player wins otherwise.
--
-- As in most trick-taking games, aces are considered the *highest* card in each
-- suit.  Otherwise, the ordering is King, Queen, Jack, and then the pip cards
-- in order by their number of pips.

leaderTakes :: Suit -> Card -> Card -> Bool
leaderTakes trump (Card pips suit) (Card pips' suit')
  | same suit suit'  = pips == 1 || pips' /= 1 && pips >= pips'
  | same suit trump  = True
  | same suit' trump = False
  | otherwise        = True

{-------------------------------------------------------------------------------

The `Maybe` datatype
--------------------

Sometimes, a value that we expect to be present may be not present; this might
either be because it's optional, or because it's missing, or because some kind
of error occurred in computing it.  Frequently, such cases are handled by using
a `null` value---Tony Hoare's billion dollar mistake.  In Haskell, we treat such
cases explicitly, using the `Maybe` type.  `Maybe` is defined by:

    data Maybe a = Nothing | Just a

For example, values of type `Maybe Int` include `Nothing`, `Just 3`, `Just 1`
and so forth.  Values of type `Maybe Suit` are `Nothing`, `Just Spades`, `Just
Hearts`, &c.

By making missing data *explicit* we guarantee both that it's handled when it
needs to be (you can't add `Maybe Int`s without deciding how to handle the
`Notthing` case), but are also explicit about when these cases *can't* occur (if
I get two `Int`s, I don't have to worry that either of them *might* be `null`).

-------------------------------------------------------------------------------}

--------------------------------------------------------------------------------
-- Problem 7: byDefault
--
-- As a warm up, let's consider a simple function that gets the value out of a
-- `Maybe`, providing a default value in case of `Nothing`.  For example,
-- `byDefault False (Just True)` should evaluate to `True`, while `byDefault 0
-- Nothing` evalutes to `0`.

byDefault :: a -> Maybe a -> a
byDefault x (Just y) = y
byDefault x Nothing  = x

--------------------------------------------------------------------------------
-- Problem 8: addMissing
--
-- One interpretation of `Nothing` is as *missing* data.  (This, incidentally,
-- is what NULL is supposed to mean in SQL.)  For this problem, you'll implement
-- addition with missing values.  If neither value is missing, then the result
-- should be the sum of the two arguments.  If either one is missing, it should
-- be treated as 0.  *But*, if both are missing, then the result stays
-- missing---you shouldn't "invent" data where you had none to start with.

addMissing :: Maybe Int -> Maybe Int -> Maybe Int 
addMissing Nothing Nothing = Nothing
addMissing i j = Just (byDefault 0 i + byDefault 0 j)

--------------------------------------------------------------------------------
-- Problem 9: addErroneous
--
-- Another interpretation of `Nothing` is as erroneous cases---that is to say,
-- something went wrong in computing a value.  For this problem, you'll
-- implement addition with erroneous values.  If *either* value is erroneous,
-- the result is also erroneous; only if neither value is erroneous should you
-- produce the sum as your answer.

addErroneous :: Maybe Int -> Maybe Int -> Maybe Int
addErroneous (Just i) (Just j) = Just (i + j)
addErroneous _ _ = Nothing

{-------------------------------------------------------------------------------

A final interpretation of `Nothing` is in optional values; `Nothing` is the case
in which no option was selected.  We're going to use this interpretation to
encode bids in Bridge: a bid consists of a number of tricks to take, and a
proposed trump suit.   However, a bidder can propose "no trump"; we'll capture
that as `Nothing`.

-------------------------------------------------------------------------------}

data Bid = Bid Int (Maybe Suit)

--------------------------------------------------------------------------------
-- Problem 10: bigger
--
-- We'll need one more helper function along the way.  In Bridge bidding, suites
-- are ordered Spades > Hearts > Diamonds > Clubs.  Implement this ordering.
-- For example, `bigger Spades Spades` should return `False`, while `bigger
-- Spades Diamonds` should return `True`

bigger :: Suit -> Suit -> Bool
bigger Spades Spades     = False
bigger Spades _          = True
bigger Hearts Spades     = False
bigger Hearts Hearts     = False
bigger Hearts _          = True
bigger Diamonds Clubs    = True
bigger Diamonds _        = False
bigger Clubs _       = False

--------------------------------------------------------------------------------
-- Problem 11: outbids
--
-- Finally, you'll implement a function to compare bridge bids.  One bid outbids
-- another if:
--
--   * It is for more tricks
--   * It is for the same number of tricks, but in a higher suit.  For these
--     purposes, "no trump" is higher than all the other suits.
--
-- For example, `outbids (Bid 5 (Just Spades)) (Bid 5 (Just Clubs))`, `outbids
-- (Bid 5 (Just Clubs)) (Bid 4 (Just Spades))`, and `outbids (Bid 5 Nothing)
-- (Bid 4 (Just Spades))` should all return `True`.

outbids :: Bid -> Bid -> Bool
outbids (Bid tricks trump) (Bid tricks' trump') = tricks > tricks' || tricks == tricks' && bigger' trump trump'
  where bigger' Nothing Nothing = False
        bigger' Nothing _ = True
        bigger' (Just s) (Just s') = bigger s s'
