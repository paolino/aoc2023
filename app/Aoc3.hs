{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module Aoc3 where

import Control.Arrow (Arrow (..))
import Data.Char (isDigit)
import Data.Either (lefts)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Text.Read (readMaybe)

main :: IO (Int, Int)
main = do
    ls <- lines <$> getContents
    let xs =
            concatMap (collect1 . fmap promote1)
                . contextualize
                . augment '.'
                $ ls
        ys =
            fmap product
                . recollect2
                . concatMap (collect2 . fmap promote2)
                . contextualize
                . augment (Right '.')
                . markGears
                $ ls
    pure (sum xs, sum ys)

-- | Add a border of the given element to the given list of lists.
augment :: x -> [[x]] -> [[x]]
augment x = (<> [repeat x]) . (repeat x :) . fmap ((<> [x]) . (x :))

type Square x = ((x, x, x), (x, x, x), (x, x, x))

-- | Blur a list of lists of elements into a list of lists of squares
contextualize :: [[c]] -> [[Square c]]
contextualize xs = fmap blur1 $ (\(x, y, z) -> zip3 x y z) <$> blur1 xs
  where
    blur1 :: [c] -> [(c, c, c)]
    blur1 ys = zip3 ys (tail ys) (tail $ tail ys)

-- | Internal type used in the collect functions
data State s r = State [Char] s [r]
    deriving (Show)

-- | Internal type for contextualized characters
type WithContext s = Maybe (Char, s)

-- | A digit pattern.
pattern Digit :: Char -> Char
pattern Digit x <- (id &&& isDigit -> (x, True))

-- | Multi digit number
type Number = Int
--------------------------------------------------------------------------------
-- Part 1
--------------------------------------------------------------------------------

promote1 :: Square Char -> WithContext Bool
promote1 ((tl, t, tr), (l, Digit x, r), (bl, b, br)) =
    Just (x, any isOk [tl, t, tr, l, r, bl, b, br])
  where
    isOk :: Char -> Bool
    isOk c = not $ isDigit c || c == '.'
promote1 (_, (_, _, _), _) = Nothing

collect1 :: [WithContext Bool] -> [Number]
collect1 = finish . foldr f (State [] False [])
  where
    f (Just (x, True)) (State ys _ xs) = State (x : ys) True xs
    f (Just (x, False)) (State ys q xs) = State (x : ys) q xs
    f Nothing s = State [] False $ finish s

    finish (State ys True xs) = number ys xs
    finish (State _ys False xs) = xs

    number :: Read a => String -> [a] -> [a]
    number = maybe id (:) . readMaybe

--------------------------------------------------------------------------------
-- Part 2
--------------------------------------------------------------------------------

type Index = Int

type TouchingGears = Set Index

-- | Either a gear index or another character
type OrGears = Either Index Char

-- | Mark the gear char with increasing numbers
markGears :: [[Char]] -> [[OrGears]]
markGears = snd . foldr f (0, [])
  where
    f row (n, rows) = (:rows) <$> foldr g (n, []) row
    g '*' (n, row) = (n + 1, Left n : row)
    g x (n, row) = (n, Right x : row)

-- | Collect all the gears around a digit
promote2 :: Square OrGears -> WithContext TouchingGears
promote2 ((tl, t, tr), (l, Right (Digit x), r), (bl, b, br)) =
    Just (x, Set.fromList $ lefts [tl, t, tr, l, r, bl, b, br])
promote2 (_, (_, _, _), _) = Nothing

-- | Collect adjacent digits into numbers with the union of their gears
collect2 :: [WithContext TouchingGears] -> [(Number, TouchingGears)]
collect2 = finish . foldr f (State [] mempty [])
  where
    f (Just (x, ys)) (State zs ys' xs) = State (x : zs) (ys <> ys') xs
    f Nothing s = State [] mempty $ finish s

    finish (State _ (null -> True) xs) = xs
    finish (State zs ys xs) = (read zs, ys) : xs

-- | Recollect the gears into a map from gear to set of numbers touching
recollect2 ::  [(Number, TouchingGears)] -> Map Index (Set Number)
recollect2 xs = Map.filter ((== 2) . length) . Map.unionsWith (<>) $ do
    (n, ks) <- xs
    k <- Set.toList ks
    pure $ Map.singleton k (Set.singleton n)
