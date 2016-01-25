{-
  See http://www.mattkeeter.com/projects/constraints/
-}

module Constraints where

import qualified Data.Map as Map
import Data.Map (Map(..))
import Data.Maybe
import Data.List

type Vars a = Map a Double

newtype Equation a = Equation { eval :: Vars a -> (Double, Vars a) }

var :: Ord a => a -> Equation a
var tag = Equation $ \vars ->
    ( Map.findWithDefault 0 tag vars
    , Map.singleton tag 1)

instance Ord a => Num (Equation a) where
    a + b = Equation $ \vars ->
        let (aval, avars) = (eval a) vars
            (bval, bvars) = (eval b) vars
        in (aval + bval, Map.unionWith (+) avars bvars)
    a * b = Equation $ \vars ->
        let (aval, avars) = eval a vars
            (bval, bvars) = eval b vars
        in ( aval * bval
           , Map.unionWith (+)
                (Map.map (* bval) avars)
                (Map.map (* aval) bvars)
           )

    abs a = Equation $ \vars ->
        let (r, ds) = eval a vars
        in  (abs r, Map.map (* signum r) ds)

    negate a = Equation $ \vars ->
        let (r, ds) = eval a vars
        in  (negate r, Map.map negate ds)

    signum a = Equation $ \vars ->
        let (r, _) = eval a vars
        in  (signum r, Map.empty)

    fromInteger a = Equation $ const (fromInteger a, Map.empty)

instance Ord a => Fractional (Equation a) where
    a / b = Equation $ \vars ->
        let (ra, das) = eval a vars
            (rb, dbs) = eval b vars
        in (ra / rb, Map.map (/ rb ** 2) $
            Map.unionWith (+)
                (Map.map (*rb) das)
                (Map.map (negate . (* ra)) dbs)
            )

    fromRational a = Equation $ const (fromRational a, Map.empty)

-- Solver implementatino

epsilon :: Double
epsilon = 1e-12

{-
  Solves a single step of gradient descent,
  using a backtracking line search.

  Returns Nothing if the descent has converged,
  otherwise Just nexPoint
-}

step :: Ord a => Equation a -> Vars a -> Maybe (Vars a)
step eqn vars =
    if r < epsilon || all ((< epsilon) . abs) (mapValues ds) || converged
    then Nothing
    else Just next
    where
        (r, ds) = eval eqn vars
        (next, converged) = backtrack 1
        threshold = 0.5 * (sum $ mapValues $ Map.map (^2) ds)
        backtrack stepSize =
            if r - r' >= stepSize * threshold
            then (vars', abs (r - r') < epsilon)
            else backtrack (stepSize * 0.5)
            where
                vars' =
                    Map.unionWith (-) vars $ Map.map (* stepSize) ds
                r' = fst (eval eqn vars')
        mapValues = map snd . Map.toList

-- Find a local minima from an Equation and a starting point
minimize :: Ord a => Equation a -> Vars a -> Vars a
minimize eqn vars =
    fromJust $ last $ takeWhile isJust
             $ iterate (step eqn =<<) (return vars)

infixl 5 ===
(===) :: Ord a => Equation a -> Equation a -> Equation a
(===) = (-)

-- Returns a list of booleans indicating constraint
-- satisfaction and a map of resulting variable values.
solveSystem :: Ord a => [Equation a] -> Vars a -> ([Bool], Vars a)
solveSystem eqns vars =
    if and satisfied
    then (satisfied, vars')
    else -- If not all constraints are satisfied, drop
         -- an unsatisfied constraint and recurse
        let index = fromJust $ elemIndex False satisfied
            (front, back) = splitAt index eqns
            (satisfied', out) =
                solveSystem (front ++ (drop 1 back)) vars'
            (a, b) = splitAt index satisfied'
        in (a ++ False:b, out)
    where
        vars'  = minimize (sum $ map (^2) eqns) vars
        scores = map (\eqn -> (fst $ eval eqn vars') ^ 2) eqns
        satisfied = map (< sqrt epsilon) scores

-- data Equation
--     = Var String Rational
--     | Const Rational
--     | Binary BinOp
--     | Unary  UnOp

-- data BinOp
--     = Plus  Equation Equation
--     | Minus Equation Equation

-- data UnOp
--     = Negate Equation