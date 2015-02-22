-- | Small Haskell exercises which should be beginner-friendly, but
-- also have scope for interesting solutions by people more familiar
-- with the language.
--
-- Later sections may build on earlier sections, and increase in
-- difficulty, but the problems within a section can be tackled in any
-- order. Don't be afraid to refer to things you've already defined!
module Problems where

import Data.Map (Map)

--------------------------------------------------------------------------------
-- Lists

-- | Get the nth element of a list.
--
-- How could you the type of this function to handle cases where the
-- index is too big?
nth :: Int -> [a] -> a
nth i xs = undefined

-- | Check how long a list is.
length :: [a] -> Int
length xs = undefined

-- | Check if a list is sorted.
sorted :: Ord a => [a] -> Bool
sorted xs = undefined

--------------------------------------------------------------------------------
-- Fibonacci Numbers
--
-- There is a potential problem with these functions based on their
-- types, can you figure out what it is? How could it be fixed?

-- | The list of all fibonacci numbers
fibs :: [Int]
fibs = undefined

-- | Get the nth fibonacci number.
fibNth :: Int -> Int
fibNth n = undefined

--------------------------------------------------------------------------------
-- Natural Numbers

-- | Natural numbers can be defined as being either zero, or one
-- greater than some other natural number.
data Nat = Zero | Succ Nat deriving Show

-- | Turn an 'Int' into a 'Nat'. If the input is less than zero,
-- return 'Nothing'.
toNat :: Int -> Maybe Nat
toNat i = undefined

-- | Check if two natural numbers are equal.
eqNat :: Nat -> Nat -> Bool
eqNat a b = undefined

-- | Compare two natural numbers.
--
-- 'Ordering' is a type with the values 'LT', 'EQ', and 'GT'. It is
-- defined in Data.Ord.
compareNat :: Nat -> Nat -> Ordering
compareNat a b = undefined

-- | Add two natural numbers.
addNat :: Nat -> Nat -> Nat
addNat a b = undefined

-- | Subtract two natural numbers.
--
-- You'll need to decide how to handle the case where the second
-- number is bigger than the first!
subNat :: Nat -> Nat -> Nat
subNat a b = undefined

-- | Multiply two natural numbers.
mulNat :: Nat -> Nat -> Nat
mulNat a b = undefined

--------------------------------------------------------------------------------
-- Lists Revisted

-- | Check if a list is longer than some amount.
longerThan :: [a] -> Int -> Bool
longerThan xs len = undefined

-- | Split a list into n-length chunks. For example:
--
-- > chunk 3 [1,2,3,4,5,6,7,8] == [[1,2,3],[4,5,6],[7,8]]
chunk :: Int -> [a] -> [[a]]
chunk len xs = undefined

-- | Perform run-length encoding of a list. For example:
--
-- > rle [1, 2, 3, 3, 4, 4] == [(1, 1) (2, 1), (3, 2), (4, 2)]
rle :: Eq a => [a] -> [(a, Int)]
rle xs = undefined

-- | Reverse run-length encoding of a list.
--
-- Is the 'Eq' constraint necessary here? Why?
unrle :: Eq a => [(a, Int)] -> [a]
unrle xs = undefined

--------------------------------------------------------------------------------
-- Propositional Logic

-- | Propositional logic is a boolean logic with a few simple rules:
data Prop =
   Not Prop
   -- ^ True if the inner proposition isn't.
  | And Prop Prop
  -- ^ True if both the inner propositions are.
  | Or Prop Prop
  -- ^ True if one of the inner propositions is.
  | Var String
  -- ^ A named variable.
  | Lit Bool
  -- ^ A literal truth value.
  deriving (Eq, Show)

-- | Construct a proposition corresponding to logical implication.
implies :: Prop -> Prop -> Prop
implies a b = undefined

-- | Get the list of unique variable names used in a 'Prop'.
vars :: Prop -> [String]
vars p = undefined

-- | Given a map from names to values, check if all of the variables
-- used in a proposition are defined. That is, the 'String's
-- associated with all 'Var's in the 'Prop' appear in the 'Map'.
--
-- 'Map' comes from Data.Map.
closed :: Map String Bool -> Prop -> Bool
closed env p = undefined

-- | Evaluate a proposition. If there are any undefined 'Var's, return
-- 'Nothing'.
evaluate :: Map String Bool -> Prop -> Maybe Bool
evaluate env p = undefined

-- | Check if a proposition is always true, regardless of the
-- environment.
tautology :: Prop -> Bool
tautology p = undefined

-- | Check if a proposition is always false, regardless of the
-- environment.
contradiction :: Prop -> Bool
contradiction p = undefined

--------------------------------------------------------------------------------
-- Binary trees
--
-- No provided type signatures or data types, try this yourself!
--
-- 1. Define a type for binary trees, where nodes can contain a value
--    of type 'Int'.
-- 2. Define equality of trees.
-- 3. Define a 'foldTree' function, in the same vein as 'foldl' or
--    'foldr'.
-- 4. Using 'foldTree', implement a function to sum the values in the
--    nodes of a tree.
-- 5. Using 'foldTree', implement a function to get the depth of the
--    largest subtree.
-- 6. Now try generalising your trees so that nodes can contain a
--    value of some arbitrary type. You will need to parameterise your
--    data type, and this will effect your function types.
