# How types can improve your domain models

This project presents different Haskell implementations of the
[ArduinoML-Kernel][], a (very) limited language of Arduino application used as
a zoo to present Domain Specific Language capabilities.

The model is defined by the following UML class diagram:

![ArduinoML abstract syntax][classDiagram]

The objective is to share some thoughts about how types can reduce the chance
of bugs without adding much complexity.

# Technical premable

This project is actually a set of markdown files that can be interpreted as
literate haskell files thanks to [markdown-unlit][].

You need to install this package to run the examples of this projects.

    $ cabal update && cabal install markdown-unlit

or
    $ stack install markdown-unlit

Then, you can browse and test the following file, preferencially in this order:

1. ['library/ArduinoML/Minimal/Model.md'][minimalModel] that provides
   a straightforward implementation of the data type required for the
   ArduinoML-Kernel language.
2. ['library/ArduinoML/Minimal/Example.md'][minimalExample] an example
   of `Application` using the previously defined model, along with a discussion
   of the limitations of this model.
3. ['library/ArduinoML/Typed/Model.md'][typedModel] an alternative data model
   with more type safety.




[markdown-unlit]: http://hackage.haskell.org/package/markdown-unlit
[ArduinoML-Kernel]: https://github.com/mosser/ArduinoML-kernel
[classDiagram]: https://raw.githubusercontent.com/mosser/ArduinoML-kernel/master/kernels/uml/ArduinoML.png
[minimalModel]: library/ArduinoML/Minimal/Model.md
[minimalExample]: library/ArduinoML/Minimal/Example.md
[typedModel]: library/ArduinoML/Minimal/Model.md
