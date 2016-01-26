module SudokuSolver where

import qualified Data.Vector as V
import Data.Vector (Vector, (!), (//))
import Data.String (unlines)
import Data.List
import Debug.Trace
import Data.Maybe
import Control.Monad (join)
import Data.Ord (comparing)

type Cell = Maybe Int

type Board  = Vector (Vector Cell)


board :: Board
board =
    let board' =
            [ [9, 1, 7, 3, 0, 0, 4, 0, 0]
            , [0, 0, 0, 0, 4, 0, 7, 1, 0]
            , [0, 0, 0, 1, 8, 0, 0, 0, 9]
            , [0, 0, 3, 0, 9, 6, 0, 2, 0]
            , [8, 9, 0, 0, 0, 0, 0, 5, 4]
            , [0, 6, 0, 5, 1, 0, 3, 0, 0]
            , [5, 0, 0, 0, 2, 4, 0, 0, 0]
            , [0, 4, 9, 0, 7, 0, 0, 0, 0]
            , [0, 0, 8, 0, 0, 3, 2, 4, 6]
            ]
        toCell x = if x == 0 then Nothing else Just x
    in  V.fromList $ map (V.fromList . map toCell) board'

showCell :: Cell -> String
showCell = maybe " " show

showRow :: Vector Cell -> String
showRow row = concat $ "|" : (map showCell' $ zip [1..] $ V.toList row) where
    showCell' (i,c) = " " ++ showCell c ++ (' ' : d) where
        d = if i `mod` 3 == 0 then "|" else " "

showBoard :: Board -> [String]
showBoard board = (hline :) $ concatMap (insertHlines) $ zip [1..] shownRows where
    insertHlines (i, r)
        | i `mod` 3 == 0 = [r, hline]
        | otherwise      = [r]
    hline = replicate rowLen '-'
    rowLen = length . head $ shownRows
    shownRows = map (showRow) $ V.toList board

printBoard = putStrLn . unlines . showBoard

chunk :: Int -> [a] -> [[a]]
chunk n xs = foldr fn [] $ zip [1..] xs where
    fn (i, x) acc
        | i `mod` n == 0 = [x]:acc
        | otherwise      =
            case acc of
                (xs:xss) -> (x:xs):xss
                _        -> error "Should never happen"

mapWithIndices fn board = (V.map . V.map) fn (boardWithIndices board)

boardWithIndices :: Board -> Vector (Vector (Int, Int, Cell))
boardWithIndices board =
    V.imap (\r row ->
        V.imap (\c cell ->
            (r,c,cell)) row) $ board

isValid :: Board -> Bool
isValid board = V.and $ V.map V.and $ mapWithIndices (isCellValid board) board

possibleCellValues' b (r, c, cell) =
    let vals = possibleCellValues b (r, c, cell)
        in (r, c, vals)

possibleCellValues :: Board -> (Int, Int, Cell) -> [Int]
possibleCellValues b (r, c, cell) =
    case cell of
        Just num  -> []
        Nothing ->
            let row = getRow r b
                col = getCol c b
                flatBlock = join $ getBlock r c b
                inValidNums = nub . map fromJust . filter isJust . V.toList . V.concat $ [row, col, flatBlock]
            in [1..9] \\ inValidNums -- \\ == difference

isCellValid :: Board -> (Int, Int, Cell) -> Bool
isCellValid b (r, c, cell) =
    let row = getRow r b
        col = getCol c b
        block = getBlock r c b
        flatBlock = join block
    in case cell of
        Nothing -> True
        Just x  ->
            let toCheck = V.concat [row, col, flatBlock]
                filtered = V.filter (== (Just x)) toCheck
            in V.length filtered == 3

getRow :: Int -> Board -> Vector Cell
getRow r board = board ! r

getCol :: Int -> Board -> Vector Cell
getCol c board = V.map (! c) board

-- expands to a 3x3 board from row -> col coords
getBlock :: Int -> Int -> Board -> Board
getBlock r c board =
    let blockRow = (r `div` 3) * 3
        blockCol = (c `div` 3) * 3
        rows = V.slice blockRow 3 board
        cols = V.map (V.slice blockCol 3) rows
    in cols

{- make a step in solving the board
   1. map board positions to possible values
   2. filter positions with no possible values
   3. sort possible values according to length of possible values (asc)
   4. insert first possible value in board
   5. do solverStep on new board
   6. if path failed continue with next possible value in board
-}
-- solverStep :: Board -> Board
solverStep board =
    let space = join $ mapWithIndices (possibleCellValues' board) board
        cleanedSpace = V.toList $ V.filter (\x -> length (thd x) > 0) space
        sortedSpace  = sortBy (comparing (length . thd)) $ cleanedSpace
    in sortedSpace

thd (a,b,c) = c