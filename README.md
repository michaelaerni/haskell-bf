# haskell-bf
Brainf**k interpreter written in Haskell. This implementation has fixed cell sizes but an unlimited array size.
Every cell is implemented as a Haskell Int and inherits those size and sign limitations.
The array can grow in upper direction, it won't grow in lower direction (if compared with a memory index).

All user input must be supplyed before the interpretation starts because the core interpreter is pure and so can't read direct console input.

## Compilation
The main module is haskell-bf.hs

Compile with:

    ghc --make -O2 haskell-bf.hs

## Testcases
The directory test/ contains some example Brainf**k code files which can be used to test the interpreter. These were copyed from external sources.

Interpret a file like

    *Main> interpretFile "test\\prime.bf" "15\n" >>= putStr
    
or compiled

    haskell-bf test\bottles.bf ""
    
Adjust the directory separator char depending on your operation system.