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
import qualified Data.SortedList as S


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
            in S.fromSortedList $ S.toSortedList $ [1..9] \\ inValidNums -- \\ == difference

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

select :: Ord s => S.SortedList s -> Maybe (s, S.SortedList s)
select states =
    S.uncons states
        -- else
        --     let h = head $ S.fromSortedList states
        --     in Just (h, states)


class (Show s, Eq s, Ord s, Ord a) => Searchable s a | s -> a where
    actions :: s -> S.SortedList a
    transition :: a -> s -> s
    -- prune :: S.SortedList s -> S.SortedList s

    search' :: s -> S.SortedList s -> (s -> Bool) -> Maybe s
    search' state frontier goalTest
        | goalTest state = Just state
        | otherwise =
            let acts = (actions state) :: S.SortedList a
                substates = S.map (flip transition $ state) acts
                next = select $ (slUnion frontier substates)
            in case next of
                Nothing -> Nothing
                Just (state', frontier') ->
                    trace (show . slLength $ frontier')
                        (search' state' frontier' goalTest)

    -- search :: s -> S.SortedList s -> (s -> Bool) -> Maybe [a]
    -- search state frontier goalTest
    --     | goalTest state = Just []
    --     | otherwise =
    --         let acts = (actions state) :: S.SortedList a
    --             substates = S.map (flip transition $ state) acts
    --             next = select $ (slUnion frontier substates)
    --         in case next of
    --             Nothing -> Nothing
    --             Just (state', frontier') -> (search state' frontier' goalTest)

    -- search :: s -> (s -> Bool) -> Maybe [a]
    -- search state goalTest =
    --     if goalTest state
    --     then Just []
    --     else
    --         let acts = (actions state) :: S.SortedList a
    --             trans a = (:) <$> Just a <*> search (transition a state) goalTest
    --             substates = S.toDescList $ S.map trans acts
    --         in if S.null acts
    --             then Nothing
    --             else join . find isJust $ substates


newtype SearchBoard = SearchBoard {unBoard :: Board}

instance Eq SearchBoard where
    (SearchBoard x) == (SearchBoard y) = x == y

instance Show SearchBoard where
    show (SearchBoard b) = unlines $ showBoard b

utility (SearchBoard b) =
    let possibilities =
            V.sum . V.map length . join
            . mapWithIndices (thd . possibleCellValues' b) $ b
        empties = V.length $ getEmptyCells b
    in empties-- ((toRational empties) / 9 * 9) / (toRational possibilities)
    -- in if empties == 0
    --    then 10000
    --    else if possibilities == 0
    --    then -10000
    --    else if possibilities < empties
    --    then -10000
    --    else (toRational possibilities) / (toRational empties)

instance Ord SearchBoard where
    compare x y = utility x `compare` utility y
        -- compare (utility x) (utility y)

instance Searchable SearchBoard (Int, Int, Int) where

    -- prune states =
    --     S.filter f states where
    --         f s = length $ getP

    actions (SearchBoard b) =
        let getActions (r, c, cell) =
                let pos = possibleCellValues b (r,c,cell)
                in  V.fromList $ map (\p -> (r,c,p)) pos
            actions = join . join $ mapWithIndices getActions b
        in if
            V.length actions < (V.length $ getEmptyCells b)
            then slEmpty
            else S.toSortedList . V.toList $ actions

    transition (r, c, val) s =
        let b' = insertCell r c (Just val) (unBoard s)
        in if isValid b'
            then SearchBoard b'
            else error "Should never happen!"

isComplete :: Board -> Bool
isComplete = V.null . getEmptyCells

insertCells :: [(Int,Int,Int)] -> Board -> Board
insertCells [] b = b
insertCells ((r,c,val):xs) b =
    let b' = insertCell r c (Just val) b
    in  trace (unlines $ ((show (r,c,val)) : showBoard b')) $ insertCells xs b'

-- solve :: Board -> Maybe Board
solve board =
    let test = isComplete . unBoard
        result = search' (SearchBoard board) slEmpty test
    in result

-- solveAndShow board =
--     let result = solve board
--     in case result of
--         Nothing   -> Nothing
--         Just acts -> Just $ insertCells acts board

getDeadEnds :: Board -> Vector (Int,Int)
getDeadEnds b =
    join . join $ mapWithIndices fn b
        where fn (r,c, Nothing) = if (length $ possibleCellValues b (r,c,Nothing)) == 0 then V.singleton (r,c) else V.empty
              fn (r,c, Just _)  = V.empty

hasSolution :: Board -> Bool
hasSolution b = (V.length $ getDeadEnds b) == 0

-- loops!? between 27-28-29
backtrack :: Int -> Board -> Maybe Board
backtrack n board =
    let empties = getEmptyCells board
        insert (n',(r, c)) =
            let pos = possibleCellValues board (r, c, Nothing)
                inserter (n'', v) = trace (show ((n, (show n') ++ " / " ++ (show $ V.length empties), n''), r,c,v, filter (> v) pos)) $ backtrack (n+1) $ insertCell r c (Just v) board
            in  join $ find isJust $ map inserter $ zip [0..] pos

    in if (isComplete board)
        then Just board
        else if (hasSolution board == False || V.length empties == 0)
            then trace "Backtrack" Nothing
            else join $ V.find isJust $ V.map insert $ V.zip (V.fromList [0.. V.length empties]) empties


thd (a,b,c) = c

solveFile file = do
    contents <- readFile file
    let sudokus = parseSudokus contents
    case sudokus of
        Left err -> putStrLn (show err)
        Right sudokus' ->
            maybe (putStrLn "unsolved") (printBoard . unBoard) (solve $ sudokus' !! 0)

-- test set

-- newtype SortedListTest = SortedListTest { unSortedListTest :: (String, Int) } deriving (Eq, Show)

-- instance Ord SortedListTest where
--     compare (SortedListTest (_,x)) (SortedListTest (_,y)) = compare x y

slDelete :: (Ord a) => a -> S.SortedList a -> S.SortedList a
slDelete x xs = S.filter (/= x) xs

slUnion :: (Ord a) => S.SortedList a -> S.SortedList a -> S.SortedList a
slUnion xs ys = union (S.fromSortedList xs) ys where
    union (x:xs) l = union xs (x `S.insert` l)
    union [] l = l

slLength :: (Ord a) => S.SortedList a -> Int
slLength = length . S.fromSortedList

slEmpty :: (Ord a) => S.SortedList a
slEmpty = S.toSortedList []

main = case backtrack 0 board1 of
    Nothing -> putStrLn ""
    Just b   -> putStrLn "" --printBoard board