module SudokuParser where

import Data.Vector (Vector)
import qualified Data.Vector as V
import Text.ParserCombinators.Parsec
import Control.Applicative ((<*),(<*>),(*>),(<$>))
import Control.Monad
import SudokuTypes


grid :: Parser Board
grid = do
    num <- string "Grid" <* spaces *> many digit
    many newline
    rows <- replicateM 9 (row <* many newline)
    return . V.fromList $ rows
    -- return $ "Grid " ++ num ++ "\n" ++ row

row :: Parser (Vector Cell)
row = replicateM 9 digit >>= (return . V.fromList . map toCell) where
    toCell c =
        let x = (read [c])
        in  if x == 0 then Nothing else Just x

parseGrid g = parse grid "Sudoku Grid" g

sudokus = many grid
parseSudokus s = parse sudokus "Sudokus" s