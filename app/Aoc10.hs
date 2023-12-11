{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Aoc10 where

import Data.Functor ((<&>))
import Data.Map (Map)
import Data.Map qualified as Map
import Prelude hiding (Left, Right)

main :: IO (Int, Int)
main = do
    xs <- getContents
    let m = Map.fromList $ do
            (y, row) <- zip [0 ..] $ lines xs
            (x, c) <- zip [0 ..] row
            pure ((x, y), charToTile c)
    let r2 =
            sum
                $ concatMap scanLine
                $ rowsOfArea
                $ markArea (run m) mempty
    pure (length (run m) `div` 2, r2)

data Tile
    = Empty
    | Tile Direction Direction
    | S
    deriving (Show, Eq)

type Coord = (Int, Int)

type Board = Map.Map Coord Tile

charToTile :: Char -> Tile
charToTile c = case c of
    '.' -> Empty
    '|' -> Tile North South
    '-' -> Tile East West
    'J' -> Tile North West
    'L' -> Tile North East
    '7' -> Tile South West
    'F' -> Tile South East
    'S' -> S
    _ -> error $ "unknown tile: " <> show c

data Direction
    = North
    | East
    | South
    | West
    deriving (Show, Eq, Ord)

opposite :: Direction -> Direction
opposite d = case d of
    North -> South
    South -> North
    East -> West
    West -> East

neighbor :: Coord -> Direction -> Coord
neighbor (x, y) d = case d of
    North -> (x, y - 1)
    South -> (x, y + 1)
    East -> (x + 1, y)
    West -> (x - 1, y)

neighborValue :: Coord -> Board -> Direction -> (Coord, Tile)
neighborValue c b d =
    let
        c' = neighbor c d
    in
        (c', Map.findWithDefault Empty c' b)

whoIsS :: Board -> (Coord, Tile)
whoIsS b =
    let
        (c, _) = Map.findMin $ Map.filter (== S) b
        allNeighbors = snd . neighborValue c b <$> [North, East, South, West]
    in
        (c,)
            $ case allNeighbors of
                [canGo North -> True, _e, canGo South -> True, _w] ->
                    Tile North South
                [_n, canGo East -> True, _s, canGo West -> True] ->
                    Tile East West
                [_n, _e, canGo South -> True, canGo West -> True] ->
                    Tile South West
                [canGo North -> True, _e, _s, canGo West -> True] ->
                    Tile North West
                [_n, canGo East -> True, canGo South -> True, _w] ->
                    Tile East South
                [canGo North -> True, canGo East -> True, _s, _w] ->
                    Tile North East
                _ -> error $ "cannot determine who is S: " <> show allNeighbors

canGo :: Direction -> Tile -> Bool
canGo d (Tile x y) = opposite d == x || opposite d == y
canGo _ _ = False

follow :: Tile -> Direction -> Direction
follow (Tile f t) d
    | f == opposite d = t
    | t == opposite d = f
    | otherwise = error $ "cannot follow " <> show d <> " on " <> show (Tile f t)
follow _ _ = error "cannot follow on empty"

run :: Board -> [(Coord, Tile)]
run b =
    let
        (c0, t0@(Tile d0 _)) = whoIsS b
        go c d = do
            let (c', t) = neighborValue c b d
            (c', t) : go c' (follow t d)
    in
        ((c0, t0) :) $ takeWhile ((/= c0) . fst) $ go c0 d0

data ATile = Up | Down | V | B

type Area = Map Coord ATile

markArea :: [(Coord, Tile)] -> Area -> Area
markArea cs a =
    foldr f a $ zip cs (tail $ cycle cs)
  where
    f ((c, _), (c', t')) = Map.insert c' (mark c c' t')
    mark (ox, oy) (x, y) t
        | ox < x = if going North t then Up else B
        | ox > x = if going South t then Down else B
        | oy < y = Down
        | oy > y = Up
        | otherwise = error "cannot mark"

going :: Direction -> Tile -> Bool
going d (Tile f t) = d == f || d == t
going _ _ = False

scanLine :: [ATile] -> [Int]
scanLine = go 0
  where
    go _ [] = []
    go n (V : xs) = go (n + 1) xs
    go _ (B : xs) = go 0 xs
    go _ (Up : xs) = go 0 xs
    go n (Down : xs) = n : go 0 xs

rowsOfArea :: Area -> [[ATile]]
rowsOfArea = cells V

--------------------------------------------------------------------------------
-- Utils
--------------------------------------------------------------------------------

cells :: a -> Map Coord a -> [[a]]
cells d a = do
    y <- [0 .. maximum yks]
    pure $ do
        x <- [0 .. maximum xks]
        pure $ Map.findWithDefault d (x, y) a
  where
    (xks, yks) = unzip $ Map.keys a

--------------------------------------------------------------------------------
-- Rendering (for debugging)
--------------------------------------------------------------------------------

renderBoard :: Board -> String
renderBoard b = unlines $ cells Empty b <&> fmap renderTile
  where
    renderTile :: Tile -> Char
    renderTile t = case t of
        Empty -> ' '
        Tile South North -> '│'
        Tile North South -> '│'
        Tile East West -> '─'
        Tile West East -> '─'
        Tile North East -> '└'
        Tile East North -> '└'
        Tile South East -> '┌'
        Tile East South -> '┌'
        Tile South West -> '┐'
        Tile West South -> '┐'
        Tile North West -> '┘'
        Tile West North -> '┘'
        S -> 'S'
        _ -> error $ "cannot render " <> show t

renderArea :: Area -> String
renderArea a = unlines $ cells B a <&> fmap renderATile
  where
    renderATile :: ATile -> Char
    renderATile t = case t of
        Up -> '↑'
        Down -> '↓'
        B -> '-'
        V -> '.'