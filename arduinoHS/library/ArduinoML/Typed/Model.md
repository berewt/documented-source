# ArduinoML, a better typed model

This file is is a Markdown file that is interpreted as a literate Haskell file.
It will use the following GHC language extension:

```haskell
{-# LANGUAGE ScopedTypeVariables #-}
```

<!--
```haskell
module ArduinoML.Typed.Model where

import Data.Bifunctor
import Data.Bifoldable
import GHC.Enum (boundedEnumFrom, boundedEnumFromThen)

data Signal = LOW | HIGH
  deriving (Read, Show, Eq, Ord, Bounded)
```
-->

This file is the third part of the
[How types can improve your domain models][arduinoHS] series, a series of
markdown files with actual running haskell code that discuss how types
can improve the definition of domain models.

In [previous file][minimalExample], I gave an example of an `Application`
using the [minimal Model][minimalModel] and discussed the limitation of
this model. I've decided to address two main issue in this new version:

1. The code should get rid of circular dependencies.
2. An application should not anymore be able to refer to bricks or state that
   are not part of this application.

## Fixing circular dependencies, the failed attempt

In the minimal model, each `State` has a `Transition` and a `Transition` has a
reference to its next step. To avoid a tight coupling between the two types,
we could have decided to write `Transition` like this:

```haskell ignore
data Transition
  = Transition
  { source :: State
  , sensor :: Sensor
  { transitionValue :: Signal
  , next :: State
  } deriving (Eq, Show, Read)
```

And then, remove `Transition` from `State` and add directly the list
of transitions in the `App` type.
Unfortunately, the initial design of the language restrict the number of
transitions for a given state to one and this restriction disappears
completely in this new version.

Another possibility would be to consider the initial version of transition
and to store the transitions directly in `App` in a `Map State Transition`
but this does not ensure us anymore that each `State` has at least one
outgoing transition.

Another possibility then would be to consider a function from `State` to
`Transition` but then we need to define a `Transition` for each `State`, even
if they're not part of the `Application`.

If only we could have more control on the value of `State`…

## The Cardinality issue

With types, a good way to control the values you can use is to design your
types accordingly. Let's try to solve the two issue with this strategy.

The idea is to have precise types for `State` and `Brick`, which have as many
inhabitants as we need to build an `Application`. But as we don't know
the number of inhabitants in advance. There's an easy way to solve it
by deffering the states and bricks type definiiton until we build an
application.

For example, for our light example, the states would be:

```haskell ignore
data State = Offline | Online
```

And wherever we need state in our data model, we can use a typeclass to
do the job.
As we face quite a similar issue with bricks, let's foresee the same solution
but, as the sensors and actuators differenciation is important, let's
consider these two as type parameters.

```haskell
data Brick sensor actuator
   = Sensor sensor
   | Actuator actuator
   deriving (Eq, Read, Show)
```

`Brick` is basically the sum of the two type parameters for actuators and sensors.
We could have used `Either` as well.

With the same approach, we can easily define actions as well:

```haskell
data Action actuator
   = Action
   { actuator :: actuator
   , newValue :: Signal
   } deriving (Eq, Read, Show)
```

And for the transitions:

```haskell
data Transition state sensor
   = Transition
   { sensor :: sensor
   , expectedValue :: Signal
   , next :: state
   }
```

At this stage, three questions are pending:
1. How do we map actions and transitions to states?
2. How do we build the application data structure?
3. Hey, did we forget names and pins?

Let consider the first question. Our initial goal when we introduced type parameters
is that, at the instanciation phase, we will pass types that are populated with
exactly all the states, sensors and actuators required for the application.
In this case, in our application, we should be able to associate each states to
a list of `Action`s and to a `Transition`.
Associate each value of a domain to a value of a codomain should ring a bell.
Yes, let's use a function!

Seeing how it can be done will answer our second question, the definition of `App`:

```haskell
data App state sensor actuator
   = App { initial :: state
         , actions :: state -> [Action actuator]
         , transitions :: state -> Transition state sensor
         }
```

That's it. Where are the list of states and Bricks? As it should be exactly the
set of possible values of your type, why should we bother to copy them explicitly
in `App`?
I'll explain later how we can deal with it.

The last point is about names and pins.
They aren't here at the moment.
We actually don't need them. Our model has become a pure logical model of the
application.
Names and pins are wiring (code generation) concerns and don't need to be
considered right now.


Before we move on, let's add a few syntactic sugars to have a more DSL'ish way
to define applications:

```haskell
(~>) :: actuator -> Signal -> Action actuator
(~>) = Action

(.==) :: sensor -> Signal -> state -> Transition state sensor
(.==) = Transition

(==>) :: (state -> Transition state sensor) -> state -> Transition state sensor
(==>) = ($)
```

## Enumerate the values of a finite type

So far I've just eluded than one of the objective of the ArduinoML-Kernel is
to generate Arduino code. To do so, we'll need to go through the states and
the bricks of the application.

Unfortunately, there is no built-in mechanism to enumerate the values of a
finite type. We could easily build one though, somethinig like:

```haskell ignore
class FiniteType a where
  cardinality :: Natural
  values :: [a]
```

With the laws:
- `genericLength values == cardinality`
- `forall x ∈ a. elem x values == True`

However, to avoid some useless complexity, it would be nice to prove a way to
derive this typeclass. I consider it as out of scope at this stage, so I
opted for a workaround that rely on existing typeclasses and derivation.
Thanks to  `Enum` and `Bounded`, we can enumerate all the values of a type
thanks to `[minBound ..]`.


Unfortunately, Haskell is not able to derive Enum and Bounded for `Brick`, so
we go with our own instances:

```haskell
instance (Enum a, Bounded a, Enum b, Bounded b) => Enum (Brick a b) where

  toEnum x | x <= fromEnum (maxBound :: a) = Sensor $ toEnum x
           | otherwise                     = Actuator $ toEnum $ x - (fromEnum (maxBound :: a) + 1)

  fromEnum (Sensor x) = fromEnum x
  fromEnum (Actuator x) = fromEnum (maxBound :: a) + 1 + fromEnum x

  enumFrom = boundedEnumFrom -- borrowed from GHC.Enum
  enumFromThen = boundedEnumFromThen -- borrowed from GHC.Enum

instance (Bounded a, Bounded b) => Bounded (Brick a b) where

  minBound = Sensor minBound
  maxBound = Actuator maxBound
```

While we're at it, let's also define `Bifunctor` and 

```haskell
instance Bifunctor Brick where

   bimap f _ (Sensor x) = Sensor $ f x
   bimap _ g (Actuator x) = Actuator $ g x

instance Bifoldable Brick where

   bifoldMap f _ (Sensor x) = f x
   bifoldMap _ g (Actuator x) = g x
```

## Next

An example, a discussion about the problem of this solution and potential solutions
to adress other issues of this data model. Just give me time to comment and commit.

[arduinoHS]: ../../../README.md
[minimalexample]: ../Minimal/Example.md
