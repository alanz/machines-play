{-# LANGUAGE TypeOperators #-} -- For :+: in signatures
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}

import Data.Char
import Data.Machine
import qualified Data.Map as Map


{-
Simplest possible machine
-------------------------

To do something useful, you need to call run,

  run :: MachineT Identity k b -> [b]

The constructor for a MachineT is

  MachineT runMachineT :: m (Step k o (MachineT m k o))

In this simplest case m is Identity, so

  MachineT runMachineT :: Identity (Step k o (MachineT Identity k o))

The `Step k o r` functor is the base functor for a Machine or
MachineT, and is normally generated via a `Plan`.


This wants a Machine generating b, and will result in a list of b,
with monadic effect

A Machine gets built from a Plan. A Plan can be considered to be built
up from one of the following primitives

 data Plan k o a
   = Done a
   | Yield o (Plan k o a)
   | forall z. Await (z -> Plan k o a) (k z) (Plan k o a)
   | Fail

This Plan is a specification for a pure Machine, that reads inputs
selected by k with types based on i, writes values of type o, and has
intermediate results of type a. (What is `i`?)


The simplest possible plan is simply

  yield b

-}

plana :: PlanT k Char m ()
plana = yield 'a'

{-
We make a Machine from the Plan. The type context must be properly
defined or the construction will fail

  construct :: Monad m => PlanT k o m a -> MachineT m k o

-}

machinea :: Monad m => MachineT m k Char
machinea = construct plana

-- |simplest machine, using Identity Monad for pure results
simplest :: [Char]
simplest = run machinea

{-
*Main> simplest
"a"
*Main>

Slightly more complex one, repeated data
----------------------------------------

type Source b = Monad m => MachineT m k b

-}
schar :: Source Char
schar = source "abcde"
{-
Note:
*Main> :t schar
schar :: Monad m => MachineT m k Char

*Main> run schar
"abcde"

A process that does not let the character b through
-}
nob :: Process Char Char
nob = filtered (/='b')
{-
*Main> :t schar ~> nob
schar ~> nob :: Monad m => MachineT m k Char
*Main> run $ schar ~> nob
"acde"
*Main>



Terminology (hand waving)

  Plan : is used to compose Machines

  Machine : is run to generate useful output. Generalisation of Source
            and Process

  Source : Machine generating output only. Effectively anchors one
           side of the processing chain with input

  Process : Machine that takes input of one type, transforms it, and
            generates output of a possible different type

  Automaton : for later investigation

More pieces
-----------

cap attaches a process to a source, generating a new source

*Main> :t cap
cap :: Monad m => Process a b -> Source a -> MachineT m k b
*Main> :t cap nob schar
cap nob schar :: Monad m => MachineT m k Char
*Main> run $ cap nob schar
"acde"
*Main>


How do we make a plan?
----------------------

From the slides

  stop :: Plan k o a

  The next primitives are used to sequence processing inside the
  Machine generated from the plan

  Generate output
    yield :: o -> Plan k o ()

  Wait for input

    await :: Category k => Plan (k i) o i
      So k must have identity and composition

    awaits ::       k i -> Plan k     o i
      Think of it as 'await specific'


Converting a plan into a Machine
--------------------------------

  construct :: Monad m => PlanT k o m a -> MachineT m k o
     Compile a machine to a model.

  repeatedly :: Monad m => PlanT k o m a -> MachineT m k o
     Generates a model that runs a machine until it stops, then start it up
     again.

  before :: Monad m => MachineT m k o -> PlanT k o m a -> MachineT m k o
     Evaluate a machine until it stops, and then yield answers according to the
     supplied model.

  Also to connect different machines

    fit :: Monad m => (forall a. k a -> k' a) -> MachineT m k o -> MachineT m k' o
      connect different kinds of machines, swapping one k function for another

    fitM :: (Monad m, Monad m') => (forall a. m a -> m' a) -> MachineT m k o -> MachineT m' k o 
      connect different kinds of machines, swapping one monad for another

    pass :: k o -> Machine k o
      Given a handle, ignore all other inputs and just stream input
      from that handle

The k functions define the sources, and how they are connected.
There are two types, a Tee and a Wye.

What is a Tee?
-------------

A 'Machine' that can read from two input stream in a deterministic
manner.

  type Tee    a b c = Machine    (T a b) c
  type TeeT m a b c = MachineT m (T a b) c
    Monadic version of Tee

The `T` data type defines the input as follows

  data T a b c where
    Constructors
      L :: T a b a
      R :: T a b b

This defines a type which can either have an L value or an R one, and
takes a function (or Monad) generating the expected output type 'a'
depending on which kind of input is presented.

-}

{-


So lets try interleaving input from two sources
-}
streama,streamb :: Machine m Char
streama = source "abcde"
streamb = source "vwxyz"

{-
:t tee streama streamb
tee streama streamb  :: Monad m => TeeT m Char Char c -> TeeT m a b c


I think the following is defined to read from two streams of Char, and
generate output of Char
-}

-- myInterleave :: Tee Char Char Char -> Machine (((->) Char) :+: ((->) Char)) Char
myInterleave :: Monad m => TeeT m Char Char c -> TeeT m a b c
myInterleave = tee streama streamb


myTee :: Tee Char Char Char
myTee = repeatedly $ do
  x <- awaits L
  yield x
  y <- awaits R
  yield y


-- myInterleave' :: Machine ((a -> Char) :+: (b -> Char)) Char

myInterleave' :: Monad m => TeeT m a b Char
myInterleave' = tee streama streamb myTee

{-

*Main> run myInterleave'
"avbwcxdyez"
*Main>

-}

-- ---------------------------------------------------------------------

{-

Exploring the Wye
-----------------

  type Wye a b c = Machine (Y a b) c
    A Machine that can read from two input stream in a non-deterministic manner.

  type WyeT m a b c = MachineT m (Y a b) c
    A Machine that can read from two input stream in a non-deterministic manner
    with monadic side-effects.


The input descriptor for a Wye or WyeT

  data Y a b c where
    Constructors
      X :: Y a b a
      Y :: Y a b b
      Z :: Y a b (Either a b)

So effectively a Wye is a heterogenous Tee, in that the two sources
can feed different data types in. This could probably be emulated by
appropriate choice of data type in a Tee


The `wye` function is used to precompose two processes onto a Wye
  wye :  Monad m =>
    ProcessT m a a' -> ProcessT m b b' -> WyeT m a' b' c -> WyeT m a b c

-}


streamn :: Machine m Int
streamn = source [1,2,3,4,5]

-- |round robin input from the Wye
myWye :: (Monad m) => MachineT m (Y Char Int) String
myWye = repeatedly $ do
  x <- awaits X
  yield [x]

  y <- awaits Y
  yield (show y)

wm :: Monad m => WyeT m Char Int String
wm = wye streama streamn myWye

{-
*Main> run wm
["a","1","b","2","c","3","d","4","e","5"]
*Main>
-}

-- | left-biased input from the Y, as per the implementation of `wye`
myWye2 :: (Monad m) => MachineT m (Y Char Int) String
myWye2 = repeatedly $ do
  x <- awaits Z
  case x of
    Left  c -> yield [c]
    Right i -> yield (show i)

wm2 :: Monad m => WyeT m Char Int String
wm2 = wye streama streamn myWye2

{-
Note: prioritises the one source over the other. This is a feature of 'wye'
*Main> run wm2
["a","b","c","d","e","1","2","3","4","5"]
*Main>
-}


main = putStrLn "done"


{-

Experimenting with state machines
----------------------------------

Moore Machine : output values are determined solely by its current state.

  data Moore a b
    Constructors
      Moore b (a -> Moore a b)


Mealy Machine : output values are determined both by its current state and
                the current inputs.

  newtype Mealy a b
    Constructors
      Mealy runMealy :: a -> (b, Mealy a b)


Construct a Moore machine from a state valuation and transition function

  unfoldMoore :: (s -> (b, a -> s)) -> s -> Moore a b

In the following examples, we use @auto@ which comes from the
@'Data.Machine.Process.Automaton'@ typeclass:

@
  auto :: k a b -> Process a b
@

This means that we can take some routing function, @k a b@, and lift it
into a process. You can think of this as a specific @'construct'@ for
automatons.
-}

-- For this next section, let's have two states and construct a simple state
-- machine the state flips every time there is a new input. We'll use this
-- state machine for the first examples of Moore and Mealy machines.

data M1State = M1A | M1B deriving (Eq,Show,Ord)


-- Construct a simple Moore machine
-- --------------------------------

-- We will arbitrarily choose a Char type for input

-- |Transition function from state M1A
m1TransitionFmA :: Char -> Moore Char M1State
m1TransitionFmA _ = Moore M1B m1TransitionFmB

-- |Transition function from state M1B
m1TransitionFmB :: Char -> Moore Char M1State
m1TransitionFmB _ = Moore M1A m1TransitionFmA

-- |Starting state and transitions for the machine
m1 :: Moore Char M1State
m1 = Moore M1A m1TransitionFmA

-- Turn the Moore state machine into a process.
--
-- Recall the definition of a process:
-- > type ProcessT m a b = MachineT m (Is a) b
m1a :: Monad m => MachineT m (Is Char) M1State
m1a = auto m1

m1m :: Monad m => MachineT m k M1State
m1m = (source "abcde") ~> m1a

{-
*Main> run m1m
[M1A,M1B,M1A,M1B,M1A,M1B]
*Main>
-}

{-

Construct a simple Mealy machine
--------------------------------

This will have two states, and every time there is an input it changes
state, and outputs the char used to transition.

  data Moore a b
    Constructors
      Moore b (a -> Moore a b)

  newtype Mealy a b
    Constructors
      Mealy runMealy :: a -> (b, Mealy a b)

-}

m2TransitionFmA :: Char -> (M1State, Mealy Char M1State)
m2TransitionFmA _ = (M1B,Mealy m2TransitionFmB)

m2TransitionFmB :: Char -> (M1State, Mealy Char M1State)
m2TransitionFmB _ = (M1A,Mealy m2TransitionFmA)

m2 :: Mealy Char M1State
m2 = Mealy m2TransitionFmA

-- Turn the Mealy state machine into a process
m2a :: Monad m => MachineT m (Is Char) M1State
m2a = auto m2

m2m :: Monad m => MachineT m k M1State
m2m = (source "abcde") ~> m2a

{-
*Main> run m2m
[M1B,M1A,M1B,M1A,M1B]
*Main>
-}

-- How is this different from the Moore machine?

--  Moore   b (a -> Moore a b)
--  Mealy (a -> (b, Mealy a b))

-- Moore gives a state, and a function mapping from input to next state
-- Mealy gives a function mapping from input to next (state,transition)

-- When they run we only see the state as output.

{-
Mealy and Moore machines can both be represented by a 6-tuple, ( S, S0, Σ, Λ, T, G ), consisting of the following:

    - a finite set of states ( S )
    - a start state (also called initial state) S0 which is an element of (S)
    - a finite set called the input alphabet ( Σ )
    - a finite set called the output alphabet ( Λ )
    - a transition function (T : S × Σ → S)
      - for Moore machines, this is a mapping from a _single state_ and the input alphabet to the next state.
      - for Mealy machines, this is a mapping from a _pair of states_ and the input alphabet to the next state.
    - an output function (G : S → Λ)
      - for Moore machines, this maps each _state_ and an input symbol to the corresponding output symbol.
      - for Mealy machines, this maps each _pair of states_ and an input symbol to the corresponding output symbol.

Sometimes it's simpler to read if you unify the transition and output functions into a single function (T : S × Σ → S × Λ)

A Moore machine can be regarded as a restricted type of finite state transducer.

-- FIXME: add a little more depth as to what a "restricted type" means and if a Mealy machine is one too

-}

{-

Mealy XOR example from https://en.wikipedia.org/wiki/Mealy_machine
[Note, seems to be an error in the diagram, two states labelled S1]

S = { S0,S1,Si}
S0 = Si
Σ = {0,1}
Λ = {0,1}
T  : S × Σ → S × Λ =
  Si 0 -> (S0,0)
  Si 1 -> (S1,0)

  S0 0 -> (S0,0)
  S0 1 -> (S1,1)

  S1 0 -> (S0,1)
  S1 1 -> (S1,0)

-}


data XIn = I0 | I1 deriving (Eq,Show,Ord)

data XOut = O0 | O1 deriving (Eq,Show)

data XState = S0 XOut | S1 XOut | Si XOut deriving (Eq,Show)


m3Mealy :: XIn -> (XState, Mealy XIn XState)
m3Mealy = m3TransitionFmSi

m3TransitionFmSi :: XIn -> (XState, Mealy XIn XState)
m3TransitionFmSi I0 = (S0 O0,Mealy m3TransitionFmS0)
m3TransitionFmSi I1 = (S1 O0,Mealy m3TransitionFmS1)

m3TransitionFmS0 :: XIn -> (XState, Mealy XIn XState)
m3TransitionFmS0 I0 = (S0 O0,Mealy m3TransitionFmS0)
m3TransitionFmS0 I1 = (S1 O1,Mealy m3TransitionFmS1)

m3TransitionFmS1 :: XIn -> (XState, Mealy XIn XState)
m3TransitionFmS1 I0 = (S0 O1,Mealy m3TransitionFmS0)
m3TransitionFmS1 I1 = (S1 O0,Mealy m3TransitionFmS1)

m3 :: Mealy XIn XState
m3 = Mealy m3TransitionFmSi

-- Turn the Mealy state machine into a process
m3a :: Monad m => MachineT m (Is XIn) XState
m3a = auto m3

m3m :: Monad m => MachineT m k XState
m3m = (source [I0,I0,I0,I1,I1,I1,I0,I0]) ~> m3a

{-
*Main> run m3m
[S0 O0,S0 O0,S0 O0,S1 O1,S1 O0,S1 O0,S0 O1,S0 O0]
*Main>
-}

-- ---------------------------------------------------------------------

-- |Transition function from state M4A
m4TransitionFmSi :: XIn -> Moore XIn XState
m4TransitionFmSi I0 = Moore (S0 O0) m4TransitionFmS0
m4TransitionFmSi I1 = Moore (S1 O0) m4TransitionFmS1

m4TransitionFmS0 I0 = Moore (S0 O0) m4TransitionFmS0
m4TransitionFmS0 I1 = Moore (S1 O1) m4TransitionFmS1

m4TransitionFmS1 I0 = Moore (S0 O1) m4TransitionFmS0
m4TransitionFmS1 I1 = Moore (S1 O0) m4TransitionFmS1


-- |Starting state and transitions for the machine
m4 :: Moore XIn XState
m4 = Moore (Si O0) m4TransitionFmSi

-- Turn the Moore state machine into a process
m4a :: Monad m => MachineT m (Is XIn) XState
m4a = auto m4

m4m :: Monad m => MachineT m k XState
m4m = (source [I0,I0,I0,I1,I1,I1,I0,I0]) ~> m4a

{-
*Main> run m4m
[Si O0,S0 O0,S0 O0,S0 O0,S1 O1,S1 O0,S1 O0,S0 O1,S0 O0]
*Main>
-}

{-
[Si O0
,S0 O0
,S0 O0
,S0 O0
,S1 O1
,S1 O0
,S1 O0
,S0 O1
,S0 O0]
-}

-- Meally [Si O0,S0 O0,S0 O0,S0 O0,S1 O1,S1 O0,S1 O0,S0 O1,S0 O0]
-- Moore        [S0 O0,S0 O0,S0 O0,S1 O1,S1 O0,S1 O0,S0 O1,S0 O0]

-- -----------------------------------------------

{-
understanding unfoldMoore
-------------------------

from https://hackage.haskell.org/package/machines-0.6.1/docs/src/Data-Machine-Moore.html
the definition of unfoldMoore is:

@
    -- | Construct a Moore machine from a state valuation and transition function
    unfoldMoore :: (s -> (b, a -> s)) -> s -> Moore a b
    unfoldMoore f = go where
      go s = case f s of
        (b, g) -> Moore b (go . g)
    {-# INLINE unfoldMoore #-}
@

Let's take a longer look at the type signature with some psuedo-haskell:

@
    unfoldMoore ::
      (s ->            -- a transition function which takes a state to generate...
        (,)            -- a combination of...
         b             -- ...whatever lives in the current state (b)
        (a -> s)       -- ...plus a function that takes seen input (a) and steps us to the next state
      )
      -> s             -- constructing a Moore requires an explicit state to kick things off
      -> Moore a b     -- and finally, we get back what we want
@

Redoing the first two state example, using XIn:
-}


m6Fm1A :: XIn -> M1State
m6Fm1A _ = M1B

m6Fm1B :: XIn -> M1State
m6Fm1B _ = M1A

fMoore :: M1State -> (M1State, XIn -> M1State)
fMoore M1A = (M1A,m6Fm1A)
fMoore M1B = (M1B,m6Fm1B)


-- So here, we can see that we can lift a function with the
-- shape of a Moore, into a Moore, if we have all the right
-- pieces
m6 :: Moore XIn M1State
m6 = unfoldMoore fMoore M1A

-- furthermoore ; D, as expected, this fits right back into our machines
-- infrastructure. No plan nessecary.
m6m :: Monad m => MachineT m k M1State
m6m = (source [I0,I1,I1,I1,I0]) ~> auto m6

{-
*Main> run m6m
[M1A,M1B,M1A,M1B,M1A,M1B]
*Main>
-}

-- ---------------------------------------------------------------------

--  newtype Mealy a b
--    Constructors
--      Mealy runMealy :: a -> (b, Mealy a b)

-- unfoldMealy :: (s -> a -> (b, s)) -> s -> Mealy a b

-- Here a is XIn,
--      b is XState

initial :: [s]
initial = []

fMealy :: [s] -> XIn -> (XState,[s])
fMealy [] = error "empty list"
-- fMealy (x:xs)


m5 :: Mealy XIn XState
m5 = unfoldMealy fMealy initial

-- | A 'Mealy' machine modeled with explicit state.
unfoldMealy1 :: (s -> a -> (b, s)) -> s -> Mealy a b
unfoldMealy1 f = go where
  go s = Mealy $ \a -> case f s a of
    (b, t) -> (b, go t)
{-# INLINE unfoldMealy1 #-}

