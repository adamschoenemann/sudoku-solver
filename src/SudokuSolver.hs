module SudokuSolver where

import qualified Data.Vector as V
import Data.Vector (Vector, (!), (//))
import Data.String (unlines)
import Data.List

type Cell = Maybe Int

type Square = Vector Cell
type Board  = Vector Square

getInSquare :: (Int, Int) -> Square -> Cell
getInSquare (x,y) sq =
    let index = 3 * y + x
    in  sq ! 3

putInSquare :: (Int, Int) -> Cell -> Square -> Square
putInSquare (x,y) num sq =
    let index = 3 * y + x
    in sq // [(index, num)]

testSqr :: Square
testSqr = V.fromList
    [ Just 1, Just 2, Nothing
    , Nothing, Just 5, Just 6
    , Nothing, Just 8, Just 9
    ]

testBoard :: Board
testBoard =
    let t = testSqr
        t' = V.fromList $ concat $ reverse $ chunk 3 $ V.toList testSqr
        t'' = V.fromList $ concat $ transpose $ chunk 3 $ V.toList testSqr
    in V.fromList
        [ t, V.reverse t, t'
        , V.reverse t', t'', V.reverse t''
        , t', t'', t
        ]

board :: Board
board =
    let tl = [9,0,1,6,0,5,2,0,0]
        tc = [6,3,2,0,9,0,5,0,0]
        tr = [8,5,7,0,2,0,0,0,9]
        ml = [0,1,0,0,0,0,3,0,0]
        mc = [0,8,3,7,0,5,1,2,0]
        mr = [0,0,2,0,0,0,0,7,0]
        bl = [8,0,0,0,3,0,1,2,9]
        bc = [0,0,9,0,1,0,8,4,6]
        br = [0,0,4,6,0,8,7,0,5]
    in V.fromList $ map (V.fromList . toSqr) [tl,tc,tr,ml,mc,mr,bl,bc,br] where
        toSqr = map (\x -> if x == 0 then Nothing else Just x)

showCell = maybe " " show

showSqr :: Square -> [String]
showSqr sqr =
    let rows =
            map (\x -> "| " ++ x ++ " |")
                $ map (concat . intersperse " | ")
                $ chunk 3 $ map showCell $ V.toList sqr
        len = length . head $ rows
        line = replicate len '-'
        divider = replicate len '='
    in (intersperse line rows) ++ [divider]


showBoard board =
    let rows = chunk 3 $ V.toList board
        squares = (map . map) showSqr rows
        divider = replicate len '='
        almostBoard = map (transpose . concat) $ (map . map) transpose squares
        len     = length $ (head . head) almostBoard
    in  [divider] : almostBoard

printBoard = mapM_ putStrLn . map (intercalate "\n") . showBoard

chunk :: Int -> [a] -> [[a]]
chunk n xs = foldr fn [] $ zip [1..] xs where
    fn (i, x) acc
        | i `mod` n == 0 = [x]:acc
        | otherwise      =
            case acc of
                (xs:xss) -> (x:xs):xss
                _        -> error "Should never happen"

isValid :: Board -> Bool
isValid = undefined

isCellValid :: Cell -> Board -> Board
isCellValid = undefined

getRow :: Int -> Board -> [Cell]
getRow r board =
    let squareOffset = r `div` 3
        squareIndices = [squareOffset .. squareOffset + 2]
        squares = map (board !) squareIndices
        rowOffset  = r `mod` 3
        rowIndices = [rowOffset .. rowOffset + 2]
        rows    = map (\square -> map (square !) rowIndices) squares
    in concat rows
