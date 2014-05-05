{-# LANGUAGE TypeOperators #-} -- For :+: in signatures
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}

import Data.Char
import Data.Machine


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


-}

{-

Construct a simple Moore machine
--------------------------------

This will have two states, and every time there is an input it changes
state.

-}

data M1State = M1A | M1B deriving (Eq,Show)

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

-- Turn the Moore state machine into a process
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

m2Mealy :: Char -> (M1State, Mealy Char M1State)
m2Mealy = m2TransitionFmA

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
A Moore machine can be defined as a 6-tuple ( S, S0, Σ, Λ, T, G ) consisting of the following:

    a finite set of states ( S )
    a start state (also called initial state) S0 which is an element of (S)
    a finite set called the input alphabet ( Σ )
    a finite set called the output alphabet ( Λ )
    a transition function (T : S × Σ → S) mapping a state and the input alphabet to the next state
    an output function (G : S → Λ) mapping each state to the output alphabet

A Moore machine can be regarded as a restricted type of finite state transducer.


A Mealy machine is a 6-tuple, (S, S0, Σ, Λ, T, G), consisting of the following:

    a finite set of states (S)
    a start state (also called initial state) S0 which is an element of (S)
    a finite set called the input alphabet (Σ)
    a finite set called the output alphabet (Λ)
    a transition function (T : S × Σ → S) mapping pairs of a state and an input symbol to the corresponding next state.
    an output function (G : S × Σ → Λ) mapping pairs of a state and an input symbol to the corresponding output symbol.

In some formulations, the transition and output functions are coalesced into a single function (T : S × Σ → S × Λ).


It seems that in this formulation the states and output alphabet have been coalesced
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

data XState = S0 | S1 | Si deriving (Eq,Show)

data XIn = I0 | I1 deriving (Eq,Show)

m3Mealy :: XIn -> (XState, Mealy XIn XState)
m3Mealy = m3TransitionFmSi

m3TransitionFmSi :: XIn -> (XState, Mealy XIn XState)
m3TransitionFmSi I0 = (S0,Mealy m3TransitionFmS0)
m3TransitionFmSi I1 = (S1,Mealy m3TransitionFmS1)

m3TransitionFmS0 :: XIn -> (XState, Mealy XIn XState)
m3TransitionFmS0 I0 = (S0,Mealy m3TransitionFmS0)
m3TransitionFmS0 I1 = (S1,Mealy m3TransitionFmS1)

m3TransitionFmS1 :: XIn -> (XState, Mealy XIn XState)
m3TransitionFmS1 I0 = (S0,Mealy m3TransitionFmS0)
m3TransitionFmS1 I1 = (S1,Mealy m3TransitionFmS1)

m3 :: Mealy XIn XState
m3 = Mealy m3TransitionFmSi

-- Turn the Mealy state machine into a process
m3a :: Monad m => MachineT m (Is XIn) XState
m3a = auto m3

m3m :: Monad m => MachineT m k XState
m3m = (source [I0,I0,I1,I1,I0,I0]) ~> m3a

{-
*Main> run m3m
[S0,S0,S1,S1,S0,S0]
*Main>
-}
