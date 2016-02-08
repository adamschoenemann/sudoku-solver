{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

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
import qualified Data.Set as S


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

class (Show s, Eq s, Ord s, Ord a) => Searchable s a where
    actions :: s -> S.Set a
    transition :: a -> s -> s

    search :: s -> (s -> Bool) -> Maybe [a]
    search state goalTest = trace (show state) $
        if goalTest state
        then Just []
        else
            let acts = (actions state) :: S.Set a
                trans a = (:) <$> Just a <*> search (transition a state) goalTest
                substates = S.toAscList $ S.map trans acts
            in if S.null acts
                then Nothing
                else join . find isJust $ substates


newtype SearchBoard = SearchBoard {unBoard :: Board} deriving Eq

instance Show SearchBoard where
    show (SearchBoard b) = unlines $ showBoard b

utility (SearchBoard b) =
    let possibilities =
            V.sum . V.map length . join
            . mapWithIndices (thd . possibleCellValues' b) $ b
        empties = V.length $ getEmptyCells b
    -- in (toRational empties) / (toRational possibilities)
    in (toRational possibilities) / (toRational empties)

instance Ord SearchBoard where
    compare x y = utility x `compare` utility y
        -- compare (utility x) (utility y)

instance Searchable SearchBoard (Int, Int, Int) where
    actions (SearchBoard b) =
        let getActions (r, c, cell) =
                let pos = possibleCellValues b (r,c,cell)
                in  V.fromList $ map (\p -> (r,c,p)) pos
        in S.fromList . V.toList . join . join . mapWithIndices getActions $ b

    transition (r, c, val) s =
        let b' = insertCell r c (Just val) (unBoard s)
        in if isValid b'
            then SearchBoard b'
            else error "Should never happen!"


insertCells :: [(Int,Int,Int)] -> Board -> Board
insertCells [] b = b
insertCells ((r,c,val):xs) b =
    let b' = insertCell r c (Just val) b
    in  trace (unlines $ ((show (r,c,val)) : showBoard b')) $ insertCells xs b'

{- make a step in solving the board
   0. check if board is solved
   1. map board positions to possible values
   2. filter positions with no possible values
   3. sort possible values according to length of possible values (asc)
       (note: this is very important for performance!)
   4. if there are no possible values, we've failed
   5. for each square
      1. for each possible value
          1. insert value in board
          2. solve resulting board

    We use join to concatenate the Maybe's (find returns a maybe)
    We exploit the lazyness of find, in that it stops the algorithm
    as soon as it hits a (Just board).
    If solve returns a (Just board) it means we've solved the sudoku!
-}
-- solve :: Board -> Maybe Board
solve board =
    let result :: Maybe [(Int,Int,Int)]
        test (SearchBoard b) = isValid b && (V.null . getEmptyCells $ b)
        result = search (SearchBoard board) test
    in result

solveAndShow board =
    let result = solve board
    in case result of
        Nothing   -> Nothing
        Just acts -> Just $ insertCells acts board




-- solve board
--     | isSolved board = trace "Solved!" $ Just board
--     | otherwise =
--         let space = sortedSpace board
--         in  if length (sortedSpace board) == 0
--             then trace "Exhausted!" Nothing
--             else
--                 let nexts = concatMap solverStep space
--                     sortedNext = reverse $ sortBy (comparing sortedSpace) nexts
--                 in  trace ("steps: " ++ (show $ length sortedNext)) $ join $ find isJust $ map solve sortedNext
--                 where
--                     solverStep (r,c, ps) =
--                         map (insertPossibility r c) ps
--                     insertPossibility r c p =
--                         insertCell r c (Just p) board


-- sortedSpace :: Board -> [(Int,Int,[Int])]
-- sortedSpace b =
--     let space = join $ mapWithIndices (possibleCellValues' b) b
--         cleanedSpace = V.toList $ V.filter (\x -> length (thd x) > 0) space
--     in  sortBy (comparing (length . thd)) $ cleanedSpace

thd (a,b,c) = c

-- solveFile file = do
--     contents <- readFile file
--     let sudokus = parseSudokus contents
--     case sudokus of
--         Left err -> putStrLn (show err)
--         Right sudokus' ->
--             maybe (putStrLn "unsolved") printBoard (solve $ sudokus' !! 1)