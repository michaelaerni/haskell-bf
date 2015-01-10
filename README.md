# haskell-bf
Brainf**k interpreter written in Haskell. This implementation has fixed cell and array sizes. Every cell is implemented as a Haskell Int and inherits those size and sign limitations. The array size must always be specified.

## Compilation
The main module is haskell-bf.hs

Compile with:

    ghc haskell-bf.hs

## Testcases
The directory test/ contains some example Brainf**k code files which can be used to test the interpreter. These were copyed from external sources.