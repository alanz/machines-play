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


