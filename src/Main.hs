module Main where

import qualified SudokuSolver as Solver
import System.Environment

main = do
    args <- getArgs
    case args of
        a:as -> Solver.main a
        []   -> putStrLn "Please provide a path to a file with sudokus"


