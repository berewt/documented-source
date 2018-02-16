# Minimal implementation of the ArduinoML language

This file is the second part of the
[How types can improve your domain models][readme] series, a series of
markdown files with actual running haskell code that discuss how types
can improve the definition of domain models.

As a reminder, here is the model we want to implement:

![ArduinoML abstract syntax][classDiagram]

This file is a exacutable Haskell file. It will use the following language
extension:

```haskell
{-# LANGUAGE DuplicateRecordFields #-}
```

<!--
```haskell
module ArduinoML.Minimal.Model where

import Data.Text (Text)
import Numeric.Natural
```
-->


Let start with the straghtforward part, the signal definition:

```haskell
data Signal = LOW | HIGH
  deriving (Read, Show, Eq, Ord, Bounded)
```

## The Subtyping issue

Translation of inheritance in a language that does not provide an inheritance
mechanism is always tricky.
There are several way to translate inheritance, depending on the intended use
of the class hierarchy.

The easiest case here is the one of `NamedElement`. It's used basically to
mutualise the use of Naame and can be done easily through a typeclass.
We don't even need a NamedElement type as it's unlikedly that we need to
regroup different type of NamedElement and to downcast them afterwards.

The `Brick` hierarchy part is a bit more complex. Haskell does not have an
inheritance mechanism, nor row polymorhism to help.
We see that:
- `Brick` is abstract;
- all the classes (`Brick`, `Sensor` and `Actuator`) classes are directly
  referenced by other classes;
- some fields are defined in the `Brick` class;
- we can assume that the hierarchy is closed (no other children class than
  `Sensor` and `Actuator` are likely to appear.

Considering all thes points, I decided to go for the following translation.
First, I define a `BrickLegacy` type, that contains all the fields of 'Brick':

```haskell
data BrickLegacy = BrickLegacy Text Natural
  deriving (Read, Show, Eq)
```

Then, I define two types for `Sensor` and `Actuator` replacing inheritance with
composition:

```haskell
data Sensor = Sensor BrickLegacy
  deriving (Read, Show, Eq)

data Actuator = Actuator BrickLegacy
  deriving (Read, Show, Eq)
```

And finally, we can define `Brick` as a sum type for `Sensor` and `Actuator`:

```haskell
data Brick
   = SensorAsBrick Sensor
   | ActuatorAsBrick Actuator
   deriving (Read, Show, Eq)
```

And, the boring part, to provide accessors with some sort of overloading,
provided by typeclasses:

```haskell
class NamedElement a where
  name :: a -> Text

instance NamedElement BrickLegacy where
  name (BrickLegacy str _) = str

instance NamedElement Sensor where
  name (Sensor b) = name b

instance NamedElement Actuator where
  name (Actuator b) = name b

instance NamedElement Brick where
  name (ActuatorAsBrick a) = name a
  name (SensorAsBrick s) = name s


class HasPin a where
  pin :: a -> Natural

instance HasPin BrickLegacy where
  pin (BrickLegacy _ n) = n

instance HasPin Sensor where
  pin (Sensor b) = pin b

instance HasPin Actuator where
  pin (Actuator b) = pin b

instance HasPin Brick where
  pin (ActuatorAsBrick a) = pin a
  pin (SensorAsBrick s) = pin s
```

## The hierarchy in action

We can now use these types to encode the other parts of the model:

```haskell
data Action
  = Action
  { actuator :: Actuator
  , value :: Signal
  } deriving (Eq, Show, Read)

data Transition
  = Transition
  { sensor :: Sensor
  , value :: Signal
  , next :: State
  } deriving (Eq, Show, Read)

data State
  = State
  { stateName   :: Text
  , actions     :: [Action]
  , transition :: Transition
  } deriving (Eq, Show, Read)

data App
  = App
  { appName      :: Text
  , initialState :: State
  , states       :: [State]
  , bricks       :: [Brick]
  } deriving (Eq, Show, Read)
```

Along with the typeclass mess to be as close as possible to the initial
model:

```haskell
instance NamedElement State where
  name = stateName

instance NamedElement App where
  name = appName
```

## The Switch example

We can now move along to the definition of a [small example][], with a
wonderful application that turns on a light when you press a button.

[readme]: ../../../..
[classDiagram]: https://raw.githubusercontent.com/mosser/ArduinoML-kernel/master/kernels/uml/ArduinoML.png
[small example]: Example.md
