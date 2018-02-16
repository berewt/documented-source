# The Switch example for the minimal model

<!--
```haskell
{-# LANGUAGE OverloadedStrings #-}
module ArduinoML.Minimal.Example

```
-->

This file is the second part of the
[How types can improve your domain models][arduinoHS] series, a series of
markdown files with actual running haskell code that discuss how types
can improve the definition of domain models.

If you haven't read it, you can jump back to the [first file][minimalModel].

## Let's code an application

This file is a small example of the use of the
[minimal data model][minimalModel] for ArduinoML. As a reminder, _minimal_
here means minimal translation efforts.

We use it to build a small application with the following requirements:
- A light is on pin 12;
- a button is on pin 8;
- when you press the button, the light is switched on;
- when you unpress the button, the light is switched off;

The code is quite straightforward:

```haskell
light :: Actuator
light = Actuator "light" 12

button :: Sensor
button = Sensor "button" 9
```

And we can use lazyness to define the state, despite their cyclic dependency.

```haskell
online :: State
online = State "online" [Action light HIGH] (Transition button LOW offline)

offline :: State
offline = State "offline" [Action light LOW] (Transition button HIGH online)
```

And then we can just put everything into an 'App':

```haskell
app :: App
app = App "Switch" offline [online, offline] [BrickSensor button, BrickActuator light]
```

## Discussion

The minimal model comes with an obvious flaw, which was mentioned in the
example above: circular dependencies are bad, and `State` and `Transition`
have a circular dependency.
While it seems natural to encode transition in the proposed way, it is still a
bad design.

Another problem is probably more subtle. One of the main advantage of types is
to restrict the domain and co-domain of our function, or, to say it in a more
trivial way, to capture some invalid values of a program at compile time instead
of sending this shit right towards the fan.
Unfortunately, this version does a poor job to this regard. Lets consider this
version of app:

```haskell
app' :: App
app' = App "Switch" offline [offline] [BrickActuator light]
```

The offline state is missing, there is no button in the state list, but it's
still a valid `App` instance.

In the next version, I'm going to tackle these two issues.
Of course, we can discuss about other valid concerns about this
implementation (invalid pin number, unreachable states, duplicate state or
brick, several Brick using the same pin,…) but these ones aren't tackled
in the next version.

Let's move on to the [typed model][].


[arduinoHS]: ../../../README.md
[minimalModel]: Model.md
[typed model]: ../Typed/Model.md
