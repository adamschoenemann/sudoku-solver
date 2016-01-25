module SudokuSolver where

import qualified Data.Vector as V
import Data.Vector (Vector, (!), (//))
import Data.String (unlines)
import Data.List
import Debug.Trace

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
    let indices = V.fromList [0..9]
    in V.map (\(r, row) ->
        V.map (\(c, cell) ->
            (r,c,cell)) $ V.zip indices row) $ V.zip indices board

isValid :: Board -> Bool
isValid board = V.and $ V.map V.and $ mapWithIndices (isCellValid board) board

isCellValid :: Board -> (Int, Int, Cell) -> Bool
isCellValid b (r, c, cell) =
    let row = getRow r b
        col = getCol c b
    in case cell of
        Nothing -> True
        Just x  ->
            let toCheck = V.concat [row, col]
                filtered = V.filter (== (Just x)) toCheck
            in V.length filtered == 2

getRow :: Int -> Board -> Vector Cell
getRow r board = board ! r

getCol :: Int -> Board -> Vector Cell
getCol c board = V.map (! c) board
