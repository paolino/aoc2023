{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Aoc14 where

import Aoc1 (reversed)
import Control.Applicative (Alternative (..))
import Data.Attoparsec.ByteString.Char8
    ( Parser
    , char
    , endOfLine
    , many1
    , parseOnly
    , sepBy
    )
import Data.ByteString qualified as B
import Data.Functor (($>))
import Data.List (transpose)
import Data.Set qualified as Set

main :: IO (Int, Int)
main = do
    xs <- B.getContents
    let Right gs = parseOnly parseGame xs
        cycling = iterate $ east . south . west . north
        (offset, r2) = untilAlreadySeen $ cycling gs
        Just ratio =
            lookup r2 $ tail $ zip (cycling r2) [0 ..]
        goal = (1_000_000_000 - offset) `mod` ratio
        r3 = cycling r2 !! goal
    pure (loadNorth $ north gs, loadNorth r3)

data W = Empty | Rock | Stone
    deriving (Show, Eq, Ord)

type Row = [W]

type Game = [Row]

parseGame :: Parser Game
parseGame = sepBy (many1 parseW) endOfLine

parseW :: Parser W
parseW = (char '.' $> Empty) <|> (char '#' $> Rock) <|> (char 'O' $> Stone)

data Accum = Accum
    { stones :: Int
    , spaces :: Int
    , output :: Row
    }

addStone :: Accum -> Accum
addStone a = a{stones = stones a + 1}

addSpace :: Accum -> Accum
addSpace a = a{spaces = spaces a + 1}

addRock :: Accum -> Accum
addRock a = a{output = Rock : output a}

emptyAccum :: Accum
emptyAccum = Accum 0 0 []

dumpAccum :: Accum -> Accum
dumpAccum Accum{..} =
    Accum 0 0
        $ replicate stones Stone
            <> replicate spaces Empty
            <> output

rollStones :: Row -> Row
rollStones = output . dumpAccum . foldr f emptyAccum
  where
    f :: W -> Accum -> Accum
    f Stone a = addStone a
    f Empty a = addSpace a
    f Rock a = addRock $ dumpAccum a

reversing :: (Row -> Row) -> Row -> Row
reversing f = reverse . f . reverse

transposing :: (Game -> Game) -> Game -> Game
transposing f = transpose . f . transpose

north :: Game -> Game
north = transposing $ fmap rollStones

west :: Game -> Game
west = fmap rollStones

south :: Game -> Game
south = transposing $ fmap (reversing rollStones)

east :: Game -> Game
east = fmap (reversing rollStones)

untilAlreadySeen :: [Game] -> (Int, Game)
untilAlreadySeen = go 0 mempty
  where
    go n seen (x : xs) =
        if x `elem` seen then (n, x) else go (n + 1) (Set.insert x seen) xs
    go _ _ _ = error "cannot see again what was not seen before"

loadNorth :: Game -> Int
loadNorth = sum . fmap loadCol . transpose

loadCol :: Row -> Int
loadCol = fst . foldr f (0, 1)
  where
    f Stone (s, e) = (s + e, e + 1)
    f _ (s, e) = (s, e + 1)

------------------------------------------------------------
-- Rendering, debugging
------------------------------------------------------------

renderGame :: Game -> String
renderGame = unlines . fmap (fmap renderW)

renderW :: W -> Char
renderW Empty = '.'
renderW Rock = '#'
renderW Stone = 'O'
