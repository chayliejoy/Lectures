{-# LANGUAGE GADTs, DataKinds, TypeOperators, KindSignatures, StandaloneDeriving #-}
module Proofs where

import Data.Kind ( Type )

import Prelude hiding (curry, uncurry)

{-------------------------------------------------------------------------------

CS:3820 Fall 2023 Lecture 11
===============================

This problem set explores the ideas of formal proof developed in lecture.  We
introduce Haskell data types corresponding to natural deduction proofs, which
enforce that proofs are properly constructed. 


We begin with the structure of propositions:

-------------------------------------------------------------------------------}

data Prop =
    TT               --   Truthity    ⊤ 
  | FF               --   Falsity     ⊥ 
  | Prop :\/: Prop   --   Disjunction ∨
  | Prop :/\: Prop   --   Conjunction ∧
  | Prop :=> Prop    --   implication → 
  deriving (Eq, Show)


-- Feel free to ignore this.
infixr 4 :/\:
infixr 3 :\/:
infixr 2 :=>

{-------------------------------------------------------------------------------

The constructors `TT` and `FF` denote truth and falsity, `:\/:` is disjunction,
`:/\:` is conjunction, and `:=>` is implication.  As mentioned in lecture, we'll
abbreviate negation using implication: to say that a proposition `P` is not
true, we'll use `P :=> FF`.

-------------------------------------------------------------------------------}

{-------------------------------------------------------------------------------

Here's the data structure for proofs.  At the top level, a term of type

    Proof g a

Is a proof of proposition `a` from the assumptions listed in `g`.  For example,
a proof that conjunction is commutative would have a type like:

    Proof '[a :/\: b] (b :/\: a)

That is: this is a proof of `b :/\: a`, that uses the assumption `a :/\: b`.

Note the single quote before the list of assumptions.  That's not optional,
sorry.

We'll talk about how the individual constructors work more as we move through
the problem set.

-------------------------------------------------------------------------------}

data Proof :: [Prop] -> Prop -> Type where
  -- Part 1: assumptions
  Zero  :: Proof (a : g) a
  Succ  :: Proof g b -> Proof (a : g) b

  -- Part 2: conjunction
  AndI  :: Proof g a -> Proof g b -> Proof g (a :/\: b)
  AndE1 :: Proof g (a :/\: b) -> Proof g a
  AndE2 :: Proof g (a :/\: b) -> Proof g b

  -- Part 3: implication
  ArrI  :: Proof (a : g) b -> Proof g (a :=> b)
  ArrE  :: Proof g (a :=> b) -> Proof g a -> Proof g b

  -- Part 3: disjunction
  OrI1  :: Proof g a -> Proof g (a :\/: b)
  OrI2  :: Proof g b -> Proof g (a :\/: b)
  OrE   :: Proof g (a :\/: b) -> Proof (a : g) c -> Proof (b : g) c -> Proof g c

  -- Part 5: negation
  TtI   :: Proof g TT
  FfE   :: Proof g FF -> Proof g a

deriving instance Show (Proof g a)  

{-------------------------------------------------------------------------------

Part 1: Assumptions
-------------------

In describing natural deduction in class, we've been informal about assumptions.
For example, we'd write a tree like

    A /\ B     A /\ B
    ------     -------
       B          A
       ------------
         B /\ A 

And observe that there's one assumption here, `A /\ B`, and we use it to prove
the conclusion `B /\ A`.  For this assignment, we're going to be more precise.
As I mentioned above, we'll track the assumptions used in the types of proofs
themselves.  The first two constructors of the `Proof` type give us ways to
access those assumptions.

The `Zero` constructor accesses the assumption at the head of the list of
assumptions.  For example, if we only have one assumption, we can use it like
this:

-------------------------------------------------------------------------------}

asmp0 :: Proof '[a] a
asmp0 = undefined

{-------------------------------------------------------------------------------

But, the `Zero` constructor works the same, regardless of how many assumptions
we've made.

-------------------------------------------------------------------------------}

asmp1 :: Proof '[a, b, c, d] a
asmp1 = undefined

{-------------------------------------------------------------------------------

The `Succ` constructor drops the first assumption in the list.  We use it to
access assumptions further in.

-------------------------------------------------------------------------------}

asmp2 :: Proof '[a, b] b
asmp2 = undefined

{-------------------------------------------------------------------------------

In essence, we access assumptions by index: the 0th one by Zero, the 1st by Succ
Zero, the 2nd by Succ (Succ Zero), &c.

A few more:

-------------------------------------------------------------------------------}


asmp4 :: Proof '[p, q, r, s] r
asmp4 = undefined

{-------------------------------------------------------------------------------

Part 2: Conjunction
-------------------

We're implementing the same rules for natural deduction here that we had in
lecture.  We have one rule to introduction a conjunction

            A        B
    (/\I)   ------------
              A /\ B 

and two rules to eliminate conjunction:

              A /\ B
    (/\E1)  ----------
                A

              A /\ B
    (/\E2)  ----------
                B

In Haskell, each of these is a constructor of the Proof type.  It has arguments
for each hypothesis, and produces a proof of the conclusion.  So, the
declaration of the introduction rule is:

    AndI :: Proof g a -> Proof g b -> Proof g (a :/\: b)

That is, given a proof of `a` and a proof of `b`, it constructs a proof of `a
:/\: b`.  We don't introduce any new assumptions, so the assumptions list `g`
stays the same.  The declaration of the first elimination rule is:

    AndE1 :: Proof g (a :/\: b) -> Proof g a

That is, given a proof of `a :/\: b`, it constructs a proof of `a`.  Again, no
chance to the assumptions.

Here's an example, showing that conjunction is commutative. 

-------------------------------------------------------------------------------}

andComm :: Proof '[a :/\: b] (b :/\: a)
andComm = undefined

{-------------------------------------------------------------------------------

Part 3: Implication
--------------------

With implication, we have to start dealing with assumptions.  The natural
deduction rule we use in lecture for implication introduction is something like
this

               A [x]
              ...
               B
    (=> I) ---------- [x]
             A => B

where we use the [x] annotation to associate the assumption `A` with the
implication.  Our Haskell version is more explicit.  Here's the declaration:

    ArrI :: Proof (a : g) b -> Proof g (a :=> b)

The overall structure is the same as the rule: we turn a proof of `b` into a
proof of `a :=> b`.  To get access to the new assumption, notice that the
subproof doesn't just have the same assumptions as the resulting proof; its
assumption list has a new first assumption, of `a`.   

Here's the trivial example of an implication:

-------------------------------------------------------------------------------}

axiom :: Proof '[] (a :=> a)
axiom = undefined

{-------------------------------------------------------------------------------

In the argument to `ArrI` (that is, the subproof), note that our assumptions
list has grown.  That's why we can access the 0th assumption, even if our
top-level proof has no assumptions.

Here's an example with multiple assumptions:

-------------------------------------------------------------------------------}                    

first :: Proof '[] (a :=> b :=> a)
first = undefined

second :: Proof '[] (a :=> b :=> b)
second = undefined

{-------------------------------------------------------------------------------

The rule for arrow elimination is simpler, as it doesn't need to do any
manipulation of assumptions.  Here's the way we've written it in lecture

          A => B    A
    (=>E) ------------
               B

and here's the corresponding declaration:

    ArrE :: Proof g (a :=> b) -> Proof g a -> Proof g b

That is: given a proofs of `a :=> b` and a proof of `a`, it constructs a proof
of `b`.  Here it is in action:

-------------------------------------------------------------------------------}

implTrans :: Proof '[] ((a :=> b) :=> (b :=> c) :=> a :=> c)
implTrans = undefined

{-------------------------------------------------------------------------------

Part 4: Disjunction
-------------------

Disjunction brings the previous pieces together.  The introduction rules are
simpler:

               A                     B
    (\/I1) ---------      (\/I2) --------
            A \/ B                A \/ B

and the corresponding declarations

    OrI1 :: Proof g a -> Proof g (a :\/: b)
    OrI2 :: Proof g a -> Proof g (a :\/: b)

`OrI1`, for instances, takes a proof of `a` and constructs a proof of `a :\/:
b`.

The complexity is in the elimination form.  Here's how we wrote it in lecture:

                          A  [x]        B  [x]
                         ...           ...
          A \/ B          C             C
    (\/E) ------------------------------------
                          C


That is to say: given a proof of `A \/ B`, and then *two* proofs of `C`, one
that assumes `A`, and one that assumes `B`, we can conclude `C`.

The declaration captures the same idea, but making the manipulation of
assumptions explicit.

    OrE :: Proof g (a :\/: b) ->
           Proof (a : g) c ->
           Proof (b : g) c ->
           Proof g c

This constructor needs three arguments.  The first is a proof of the disjunction
`a :\/: b`.  The second is a proof of `c`; note that the assumptions list has
been extended with a new assumption of `a`.  The third is another proof of `c`,
in which there is an assumption of `b`.  Given all three of these, we construct
a proof of c`.

Here it is in action:

-------------------------------------------------------------------------------}

orComm :: Proof '[] (a :\/: b :=> b :\/: a)
orComm = undefined

{-------------------------------------------------------------------------------

Part 5: Negation
----------------

Finally, we turn to negation.  In this approach, negation is really just an
abbreviation: when we say "not A", what we mean is that "A implies false".  We
can write this in Haskell as well:

-------------------------------------------------------------------------------}

type Not p = p :=> FF

{-------------------------------------------------------------------------------

This means that the important thing to know is what you can do with false.  The
natural deduction rule we wrote is

           ⊥
    (⊥E) -----
           A

That is to say, if you've managed to prove false, you can draw any other
conclusions that you want.  The constructor for false elimination does the same
thing:

    FfE :: Proof g FF -> Proof g a

Given a proof of `FF`, this constructs a proof of anything else you want.

Here's an example.  ECQ (Latin for "ex contradictione quodlibet"---from a
contradiction, anything follows) says that if you know both A and not A, you can
conclude any B.  Note that we're relying on `Not a` meaning the same thing as `a
:=> FF`, so we can apply the `ArrE` rules.

-------------------------------------------------------------------------------}


ecq :: Proof '[] (a :/\: Not a :=> b)
ecq = undefined

{-------------------------------------------------------------------------------

Here are two classic results about negation.  Again, you can use earlier proofs
in the file if you want.

-------------------------------------------------------------------------------}

modusTollens :: Proof '[] ((a :=> b) :=> Not b :=> Not a)
modusTollens = undefined

deMorgan :: Proof '[] (Not (a :\/: b) :=> Not a :/\: Not b)
deMorgan = undefined

{-------------------------------------------------------------------------------


Double negation states that

  ¬ ¬ A ≡ A.

In this logic, we have *one* direction of the traditional interpretation of
double-negation: we have that

  A ⇒ ¬ ¬ A

but not that

  ¬ ¬ A ⇒ A.

We show a proof of the first.

-------------------------------------------------------------------------------}

dni :: Proof '[] (a :=> Not (Not a))
dni = undefined
