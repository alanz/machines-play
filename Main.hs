{-# LANGUAGE TypeOperators #-} -- For :+: in signatures
import Data.Machine

{-
Simplest possible machine
-------------------------

To do something useful, you need to call run, 

  run :: Control.Monad.MonadPlus m => Machine m b -> m [b]

This wants a Machine generating b, and will result in a list of b,
with monadic effect

A Machine gets built from a Plan. The simplest possible plan is simply

  yield a
-}

plana :: Plan Char IO ()
plana = yield 'a'

{-
We make a Machine from the Plan. The type context must be properly
defined or the construction will fail

  construct :: Plan o m a -> Machine m o
  construct plana :: Machine IO Char
-}

simplest :: IO [Char]
simplest = run $ construct plana

{-
*Main> simplest
"a"
*Main>

Slightly more complex one, repeated data
----------------------------------------
-}
schar :: Source Char
schar = source "abcde"
{-
Note:
*Main> :t schar
schar :: Machine m Char

*Main> run schar
"abcde"

A process that does not let the character b through
-}
nob :: Process Char Char
nob = filtered (/='b')
{-
*Main> :t schar ~> nob
schar ~> nob :: Machine m Char
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
cap :: Process a b -> Source a -> Machine m b
*Main> :t cap nob schar
cap nob schar :: Machine m Char
*Main> run $ cap nob schar
"acde"
*Main>


How do we make a plan?
----------------------

From the slides

  stop seems to have disappeared

  The next primitives are used to sequence processing inside the
  Machine generated from the plan

  Generate output
    yield :: o -> Plan o m ()

  Wait for input
    await :: Await i f => f i
    awaits :: Await i f => (f i -> g j) -> Plan o g j

  No idea what this does
    request :: g j -> Plan o g j


Converting a plan into a Machine
--------------------------------

  construct :: Plan o m a -> Machine m o
  repeatedly :: Plan o m a -> Machine m o
  before :: Machine m o -> Plan o m a -> Machine m o

  Also to connect different machines

    fit :: (forall a. m a -> n a) -> Machine m o -> Machine n o
    pass :: m o -> Machine m o
 
What is a Tee?
-------------

A 'Machine' that can read from two input stream in a deterministic
manner.

Firstly

-- | The input descriptor for a 'Tee' or 'TeeT'.
data (m :+: n) a = L (m a) | R (n a)
  deriving (Functor, Foldable, Traversable)

This defines a type which can either have an L value or an R one, and
takes a function (or Monad) generating the expected output type 'a'
depending on which kind of input is presented.

-}
tinput :: ( IO :+: ((->) Int)) Char -> Char
tinput (L _) = 'a'
tinput (R _) = 'b'

{-


So lets try interleaving input from two sources
-}
streama,streamb :: Machine m Char
streama = source "abcde"
streamb = source "vwxyz"
{-
:t tee streama streamb
tee streama streamb :: Tee Char Char c -> Machine (m :+: n) c


I think the following is defined to read from two streams of Char, and
generate output of Char
-}

myInterleave :: Tee Char Char Char -> Machine (((->) Char) :+: ((->) Char)) Char
myInterleave = tee streama streamb 

myTee :: Tee Char Char Char
myTee = repeatedly $ do
  x <- request (L (const 'a'))
  yield 'a'
  y <- request (R (const 'b'))
  yield 'b'

-- myInterleave' :: Machine ((a -> Char) :+: (b -> Char)) Char
myInterleave' :: Machine (m :+: n) Char
myInterleave' = tee streama streamb myTee

mm :: Machine m Char
mm = fit cappedT myInterleave'
{-

*Main> run mm
"abababababa"
*Main>

-}

-- Copied from Data.Machine.Tee
-- | Natural transformation used by 'capL' and 'capR'.
cappedT :: (f :+: f) a -> f a
cappedT (R f) = f
cappedT (L f) = f


main = putStrLn "done"


