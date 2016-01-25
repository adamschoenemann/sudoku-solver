module FD where
-- {-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- {-# LANGUAGE RankNTypes #-}

-- module FD (
--         -- Types
--         FD,           -- Monad for finite domain constraint solver
--         FDVar,        -- Finite domain solver variable
--         -- Functions
--         runFD,        -- Run the monad and return a list of solutions.
--         newVar,       -- Create a new FDVar
--         newVars,      -- Create multiple FDVars
--         hasValue,     -- Constrain a FDVar to a specific value
--         same,         -- Constrain two FDVars to be the same
--         different,    -- Constrain two FDVars to be different
--         allDifferent, -- Constrain a list of FDVars to be different
--         (.<.),        -- Constrain one FDVar to be less than another
--         labelling     -- Backtracking search for all solutions
--     ) where


-- import Prelude hiding (lookup)
-- import Control.Monad.State.Lazy
-- import Control.Monad.Trans
-- import qualified Data.Map as Map
-- import Data.Map ((!), Map)
-- import qualified Data.IntSet as IntSet
-- import Data.IntSet (IntSet)

-- -- The FD monad
-- newtype FD s a = FD { unFD :: StateT (FDState s) [] a }
--     deriving (Monad, MonadPlus, MonadState (FDState s))

-- -- FD variables
-- newtype FDVar s = FDVar { unFDVar :: Int } deriving (Eq, Ord)

-- type VarSupply s = FDVar s
-- data VarInfo s = VarInfo
--     { delayedConstraints :: FD s (), values :: IntSet }

-- type VarMap s = Map (FDVar s) (VarInfo s)
-- data FDState s = FDState
--     { varSupply :: VarSupply s, varMap :: VarMap s}

-- -- Run the FD monad and produce a lazy list of possible solutions.
-- runFD :: (forall s. FD s a) -> [a]
-- runFD fd = evalStateT (unFD fd) initState

-- initState :: FDState s
-- initState = FDState { varSupply = FDVar 0, varMap = Map.empty }

-- newVar :: [Int] -> FD s (FDVar s)
-- newVar domain = do
--     v <- nextVar
--     v `isOneOf` domain
--     return v
--     where
--         nextVar :: FD s (FDVar s)
--         nextVar = do
--             s <- get
--             let v = varSupply s
--             put $ s { varSupply = FDVar (unFDVar v + 1) }
--             return v
--         isOneOf :: FDVar s -> [Int] -> FD s ()
--         x `isOneOf` domain =
--             modify $ \s ->
--                 let vm = varMap s
--                     vi = VarInfo
--                             { delayedConstraints = return ()
--                             , values = IntSet.fromList domain
--                             }
--                 in  s { varMap = Map.insert x vi vm }

-- newVars :: Int -> [Int] -> FD s [FDVar s]
-- newVars n domain = replicateM n (newVar domain)

-- -- Lookup the current domain of a variable
-- lookup :: FDVar s -> FD s IntSet
-- lookup x = do
--     s <- get
--     return . values $ varMap s ! x

-- -- Update the domain of a variable and fire all delayed constraints
-- -- associated with that variable
-- update :: FDVar s -> IntSet -> FD s ()
-- update x i = do
--     s <- get
--     let vm = varMap s
--     let vi = vm ! x
--     put $ s { varMap = Map.insert x (vi { values = i }) vm }
--     delayedConstraints vi

-- -- Add a new constraint for a variable to the constraint store
-- addConstraint :: FDVar s -> FD s () -> FD s ()
-- addConstraint x constraint = do
--     s <- get
--     let vm = varMap s
--     let vi = vm ! x
--     let cs = delayedConstraints vi
--     put $ s { varMap =
--                 Map.insert x
--                     (vi { delayedConstraints = cs >> constraint })
--                     vm
--             }

-- -- Useful helper function for adding binary constraints between FDVars.
-- type BinaryConstraint s = FDVar s -> FDVar s -> FD s ()
-- addBinaryConstraint :: BinaryConstraint s -> BinaryConstraint s
-- addBinaryConstraint f x y = do
--     let constraint = f x y
--     constraint
--     addConstraint x constraint
--     addConstraint y constraint
