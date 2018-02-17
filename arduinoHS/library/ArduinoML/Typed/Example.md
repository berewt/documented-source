# The Switch example for the typed model

<!--
```haskell
{-# LANGUAGE LambdaCase #-}
module ArduinoML.Typed.Example where

import ArduinoML.Typed.Model
```
-->

This file is the second part of the
[How types can improve your domain models][arduinoHS] series, a series of
markdown files with actual running haskell code that discuss how types
can improve the definition of domain models.

If you haven't read it, you can jump back to the [previous file][typedModel].

## An application detailled syntax.

First we need to define our bricks, we need a type for sensors and one for
actuators:

```haskell
data Actuator = Light
  deriving (Eq, Ord, Enum, Bounded)

data Sensor = Button
  deriving (Eq, Ord, Enum, Bounded)
```

There's not that much to say about theses instances, as we deferred most
of their information to functions, they're just naked values.
It's the same for states:

```haskell
data State = Online | Offline
  deriving (Eq, Ord, Enum, Bounded)
```

Then, to build our application, we need to define actions and transitions
that are basically functions from the domain of `State`, using the DSL syntax:

```haskell ignore
myActions :: State -> [Action Actuator]
myActions Offline = [Light ~> LOW]
myActions Online  = [Light ~> HIGH]

myTransitions :: State -> Transition State Sensor
myTransitions Offline = Button .== HIGH ==> Online
myTransitions Online  = Button .== LOW ==> Offline
```

And we finish with the application:

```haskell ignore
myApp :: App State Sensor Actuator
myApp = App { initial = Offline
            , transitions = myTransitions
            , actions = myActions
            }
```

Nice but verbose. Let see if we can compact it.

## The compact syntax

We can get directly internalise `myTransitions` and `myActions` into `myApp`
using an anonymous function.
We can even use the `LambdaCase` syntax to have something cleaner, in the end,
this would be sufficient:

```haskell
myApp :: App State Sensor Actuator
myApp = App
  { initial = Offline
  , transitions = \case Offline -> Button .== HIGH ==> Online
                        Online  -> Button .== LOW ==> Offline
  , actions = \case Offline -> [Light ~> LOW]
                    Online  -> [Light ~> HIGH]
  }
```

## Discussion

While we provide more safety thanks to type, the proposed version still has
some drawbacks.
First, we'll have to find a way to enumerate the values of a type when we'll
want to wire our model.
There is no built-in mechanism in Haskell to iterate over all the value of the
type.
As a consequence, we'll have to rely on the `Enum` and `Bounded` typeclasses,
which can look artificial in this context.

Second, compared to the initial encoding, it will be a lot harder with this one
to dynamically load a model.
The need of precise types to build a model is a major hurdle.
As a consequence, this model would better fit an embedded DSL scenario.

Last, two different applications are likely to have different types.
As a consequence, if you want to build functions that requires several
applications, you may face issues.
I may talked about this issue in a next post, explaining how
_datatypes à la carte_ can be used to solve some of these issues.


[arduinoHS]: ../../../README.md
[typedModel]: Model.md
