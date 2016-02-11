# Sudoku Solver
This is a simple backtracking sudoku solver I wrote in Haskell, just for
fun.

It produces a binary, called solver, that takes a single argument which is
a path to a file containing sudokus. The file `sudoku.txt` in this repo
is an example of such a file.

## Usage
1. clone
2. `cabal install`
3. `dist/build/solver/solver ./sudoku.txt`
