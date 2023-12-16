{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Aoc16 where

import Control.Applicative (Alternative (..))
import Data.Attoparsec.ByteString.Char8
    ( Parser
    , char
    , endOfLine
    , many1
    , parseOnly
    , sepBy1
    )
import Data.ByteString qualified as B
import Data.Foldable (foldl')
import Data.Functor (($>))
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set

main :: IO (Int, Int)
main = do
    xs <- B.getContents
    let Right gs = parseOnly parseGameRaw xs
        g = mkGame gs
        b0 = (E, (-1, 0))
        boot (d, c) = stepBeam g d c
        solve xs' = maximum $ run g . boot <$> xs'
    pure (solve [b0], solve $ frame g)

data Cell = Empty | Vertical | Horizontal | Slash | Backslash
    deriving (Show, Eq, Ord)

type Coo = (Int, Int)

type Game = Map Coo Cell

parseGameRaw :: Parser [[Cell]]
parseGameRaw = sepBy1 parseLine endOfLine

parseLine :: Parser [Cell]
parseLine = many1 parseCell

parseCell :: Parser Cell
parseCell =
    (char '.' $> Empty)
        <|> (char '|' $> Vertical)
        <|> (char '-' $> Horizontal)
        <|> (char '/' $> Slash)
        <|> (char '\\' $> Backslash)

mkGame :: [[Cell]] -> Game
mkGame =
    Map.fromList
        . concat
        . zipWith (\y -> zipWith (\x c -> ((x, y), c)) [0 ..]) [0 ..]

data Dir = N | E | S | W
    deriving (Show, Eq, Ord)

type Beams = Map Coo (Set Dir)

stepBeam :: Game -> Dir -> Coo -> Beams
stepBeam g d p = maybe mempty (uncurry Map.singleton) $ do
    let p' = next d p
    c <- Map.lookup p' g
    pure (p', Set.fromList $ step d c)

mergeBeams :: Beams -> Beams -> Beams
mergeBeams = Map.unionWith Set.union

expandBeams :: Game -> Beams -> Beams
expandBeams g bs = Map.unionsWith Set.union $ do
    (p, ds) <- Map.toList bs
    d <- Set.toList ds
    pure $ stepBeam g d p

data State = State
    { past :: Beams
    , frontier :: Beams
    }
    deriving (Show, Eq, Ord)

expand :: Game -> State -> State
expand g (State past' frontier') = State{..}
  where
    frontier'' = expandBeams g frontier'
    past = mergeBeams past' frontier'
    frontier = Map.fromList $ do
        (p, ds) <- Map.toList frontier''
        let ds' = Set.difference ds $ Map.findWithDefault mempty p past
        if null ds' then [] else pure (p, ds')

expansions :: Game -> State -> [State]
expansions g bs = bs : expansions g (expand g bs)

next :: Dir -> Coo -> Coo
next d (x, y) = case d of
    N -> (x, y - 1)
    E -> (x + 1, y)
    S -> (x, y + 1)
    W -> (x - 1, y)

step :: Dir -> Cell -> [Dir]
step N = \case
    Horizontal -> [W, E]
    Slash -> [E]
    Backslash -> [W]
    _ -> [N]
step E = \case
    Vertical -> [N, S]
    Slash -> [N]
    Backslash -> [S]
    _ -> [E]
step S = \case
    Horizontal -> [W, E]
    Slash -> [W]
    Backslash -> [E]
    _ -> [S]
step W = \case
    Vertical -> [N, S]
    Slash -> [S]
    Backslash -> [N]
    _ -> [W]

run :: Game -> Beams -> Int
run g b =
    length
        . past
        . head
        . dropWhile (not . null . frontier)
        $ expansions g (State mempty b)

frame :: Game -> [(Dir, Coo)]
frame g = north <> east <> south <> west
  where
    (maxX, maxY) = (maximum $ fst <$> Map.keys g, maximum $ snd <$> Map.keys g)
    north = (\x -> (S, (x, -1))) <$> [0 .. maxX]
    east = (\y -> (W, (maxX + 1, y))) <$> [0 .. maxY]
    south = (\x -> (N, (x, maxY + 1))) <$> [0 .. maxX]
    west = (\y -> (E, (-1, y))) <$> [0 .. maxY]

--------------------------------------------------------------------------------
-- Rendering, debugging
--------------------------------------------------------------------------------

renderBeams :: Beams -> String
renderBeams bs = unlines $ do
    y <- [0 .. 10]
    pure $ do
        x <- [0 .. 10]
        pure $ case Map.lookup (x, y) bs of
            Nothing -> '.'
            Just _ -> '#'

renderCells :: Game -> String
renderCells g = unlines $ do
    y <- [0 .. maxY]
    pure $ do
        x <- [0 .. maxX]
        pure $ case Map.lookup (x, y) g of
            Nothing -> '.'
            Just Empty -> '.'
            Just Vertical -> '|'
            Just Horizontal -> '-'
            Just Slash -> '/'
            Just Backslash -> '\\'
  where
    (maxX, maxY) =
        foldl' (\(x, y) (x', y') -> (max x x', max y y')) (0, 0)
            $ Map.keys g