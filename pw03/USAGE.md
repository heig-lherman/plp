# How to use

To allow for unit testing and easier library development, this project has been made as a `cabal` project.

`cabal build` will compile a binary.

`cabal run csvkit` will allow you to run the executable, to use program arguments add the following: `cabal run csvkit -- <arguments>`

`cabal test` will run the unit tests.

To test the library in the interpreter, run `cd lib && ghci CSV`.
