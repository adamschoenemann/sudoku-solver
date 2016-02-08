module SudokuTypes where

import Data.Vector

type Cell = Maybe Int

type Board  = Vector (Vector Cell)