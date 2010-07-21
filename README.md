h1. Haskell call graph builder

Currently this is very hacked up and only designed to be run on the output of the Cambridge Haskell Supercompiler. It is very
useful for visualising the structure of the recursion in the output programs -- in particular, it can find where the inner loops are.


h2. Usage

Simply run with the name of the Haskell source file(s) to analyse:

    haskell-call-graph Foo.hs Bar.hs

The output will be saved in `Foo.png` and `Bar.png` respectively.
