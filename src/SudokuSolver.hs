{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module SudokuSolver where

import qualified Data.Vector as V
import Data.Vector (Vector, (!), (//))
import Data.String (unlines)
import Data.List
import Debug.Trace
import Data.Maybe
import Control.Monad (join)
import Data.Ord (comparing)
import Control.Applicative
import SudokuTypes
import SudokuParser


board1 :: Board
board1 =
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

board2 :: Board
board2 =
    let board' =
            [ [6,9,3,7,2,5,8,1,4]
            , [7,8,2,0,6,4,5,3,9]
            , [1,0,5,8,9,3,7,2,6]
            , [2,3,8,0,5,6,1,4,7]
            , [5,7,9,4,1,2,6,8,3]
            , [0,6,1,0,7,8,0,9,5]
            , [8,1,7,5,3,9,4,6,2]
            , [3,5,6,2,4,1,9,7,8]
            , [9,2,4,6,8,7,3,5,1]
            ]
        toCell x = if x == 0 then Nothing else Just x
    in  V.fromList $ map (V.fromList . map toCell) board'

board3 :: Board
board3 =
    let board' =
            [ [6,0,0,7,2,5,8,1,4]
            , [7,0,2,0,6,4,5,3,9]
            , [1,0,5,0,9,3,7,2,6]
            , [2,3,0,0,5,6,1,4,7]
            , [5,7,9,4,1,2,6,8,3]
            , [0,6,1,0,0,8,0,9,5]
            , [8,1,7,5,3,9,0,6,2]
            , [3,5,6,2,4,1,0,7,8]
            , [9,2,4,0,8,0,3,5,1]
            ]
        toCell x = if x == 0 then Nothing else Just x
    in  V.fromList $ map (V.fromList . map toCell) board'


board4 :: Board
board4 =
    let board' =
            [ [0,0,3,0,2,0,6,0,0]
            , [9,0,0,3,0,5,0,0,1]
            , [0,0,1,8,0,6,4,0,0]
            , [0,0,8,1,0,2,9,0,0]
            , [7,0,0,0,0,0,0,0,8]
            , [0,0,6,7,0,8,2,0,0]
            , [0,0,2,6,0,9,5,0,0]
            , [8,0,0,2,0,3,0,0,9]
            , [0,0,5,0,1,0,3,0,0]
            ]
        toCell x = if x == 0 then Nothing else Just x
    in  V.fromList $ map (V.fromList . map toCell) board'



board5 :: Board
board5 =
    let board' =
            [ [5,3,0,0,7,0,0,0,0]
            , [6,0,0,1,9,5,0,0,0]
            , [0,9,8,0,0,0,0,6,0]
            , [8,0,0,0,6,0,0,0,3]
            , [4,0,0,8,0,3,0,0,1]
            , [7,0,0,0,2,0,0,0,6]
            , [0,6,0,0,0,0,2,8,0]
            , [0,0,0,4,1,9,0,0,5]
            , [0,0,0,0,8,0,0,7,9]
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

-- mapWithIndices :: (a -> b) -> Vector (Vector a) -> Vector (Vector b)
mapWithIndices fn board = (V.map . V.map) fn (boardWithIndices board)

boardWithIndices :: Board -> Vector (Vector (Int, Int, Cell))
boardWithIndices board =
    V.imap (\r row ->
        V.imap (\c cell ->
            (r,c,cell)) row) $ board

isValid :: Board -> Bool
isValid board = V.and $ V.map V.and $ mapWithIndices (isCellValid board) board

-- returns possible cell values along with coordinates
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

getEmptyCells :: Board -> Vector (Int,Int)
getEmptyCells =
    V.map (\(r,c,x) -> (r,c)) . V.filter (isNothing . thd) . join . boardWithIndices

isSolved :: Board -> Bool
isSolved board =
    let unmarked = getEmptyCells board
    in  V.length unmarked == 0

insertCell :: Int -> Int -> Cell -> Board -> Board
insertCell r c cell board =
    let row = board ! r
        x = row ! c
    in case x of
        Just _ -> error "Dont insert where is already a number"
        Nothing ->
            let row' = row // [(c, cell)]
                board' = board // [(r, row')]
            in board'

isComplete :: Board -> Bool
isComplete = V.null . getEmptyCells

insertCells :: [(Int,Int,Int)] -> Board -> Board
insertCells [] b = b
insertCells ((r,c,val):xs) b =
    let b' = insertCell r c (Just val) b
    in  trace (unlines $ ((show (r,c,val)) : showBoard b')) $ insertCells xs b'



getDeadEnds :: Board -> Vector (Int,Int)
getDeadEnds b =
    join . join $ mapWithIndices fn b
        where
            fn (r,c, Nothing) =
                if (length $ possibleCellValues b (r,c,Nothing)) == 0
                then V.singleton (r,c) else V.empty
            fn (r,c, Just _)  = V.empty

hasSolution :: Board -> Bool
hasSolution b = (V.length $ getDeadEnds b) == 0


backtrack :: Board -> Maybe Board
backtrack board
    | isComplete board = Just board
    | hasSolution board = forward board empties
    | otherwise = Nothing
    where
        empties  = V.toList $ getEmptyCells board
        tryVal _ [] b = Nothing
        tryVal (r,c) (v:vals) b =
            case backtrack (insertCell r c (Just v) b) of
                Nothing -> tryVal (r,c) vals b
                Just res -> Just res
        forward b ((r,c):positions) =
            let vals = possibleCellValues b (r,c, Nothing)
            in  tryVal (r,c) vals b

thd (a,b,c) = c

solveFile file = do
    contents <- readFile file
    let sudokus = parseSudokus contents
    case sudokus of
        Left err -> putStrLn (show err)
        Right sudokus' ->
            let solve (i,s) = do
                    putStrLn (show i)
                    maybe (putStrLn "unsolved") (printBoard)
                        (backtrack s)
            in mapM_ solve $ zip [0..] sudokus'

solve board = backtrack board

main = solveFile