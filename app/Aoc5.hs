{-# LANGUAGE DeriveFunctor #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Aoc5 where

import Control.Monad (void)
import Data.Attoparsec.ByteString.Char8
    ( Parser
    , char
    , decimal
    , many1
    , parseOnly
    , satisfy
    , sepBy1
    , string
    )
import Data.ByteString.Char8 qualified as B
import Data.Char (isAlpha)
import Data.Functor ((<&>))
import Data.Map (Map, unions)
import Data.Map qualified as Map

main :: IO (Int, Int)
main = do
    xs <- B.getContents
    let Right game = parseOnly parseGame xs
        seeds' = seeds game
        mappings' = mappings game
        r1 = seeds' <&> \s -> run1 "seed" s mappings'
        r2 = run2 "seed" $ convertGame game
    return (minimum $ last <$> r1, minimum $ fst <$> r2)

type Target = String

type Mapping = Map Int Int

data MappingTo m = MappingTo
    { target :: Target
    , mapping :: m
    }
    deriving (Show, Functor)

type Mappings m = Map Target (MappingTo m)

data Game r m = Game
    { seeds :: [r]
    , mappings :: Mappings m
    }
    deriving (Show)

--------------------------------------------------
-- parsing
--------------------------------------------------

parseSeeds :: Parser [Int]
parseSeeds = do
    _ <- string "seeds: "
    sepBy1 decimal (char ' ') <* newline

parseMapping :: Parser (Mappings Mapping)
parseMapping = do
    [from, _, to] <- sepBy1 (many1 $ satisfy isAlpha) (char '-')
    string " map:" <* newline
    (mappings', defaults) <- unzip <$> sepBy1 parseSingleMapping newline
    newline
    pure
        $ Map.singleton from
        $ MappingTo to
        $ unions mappings' <> Map.unions defaults

newline :: Parser ()
newline = void (char '\n')

parseSingleMapping :: Parser (Mapping, Mapping)
parseSingleMapping = do
    [to :: Int, from, range] <- sepBy1 decimal (char ' ')
    pure (Map.singleton from (to - from), Map.singleton (from + range) 0)

parseGame :: Parser (Game Int Mapping)
parseGame = do
    seeds' <- parseSeeds <* newline
    mappings' <- sepBy1 parseMapping newline
    pure $ Game seeds' $ Map.unions mappings'

--------------------------------------------------
-- part 1
--------------------------------------------------

fromTo :: Int -> Mapping -> Int
fromTo x m = case Map.lookupLE x m of
    Just (_k, v) -> x + v
    Nothing -> x

run1 :: Target -> Int -> Mappings Mapping -> [Int]
run1 target' k mappings' = case Map.lookup target' mappings' of
    Just (MappingTo target'' m') -> k : run1 target'' (fromTo k m') mappings'
    Nothing -> [k]

--------------------------------------------------
-- part 2
--------------------------------------------------

type Range = (Int, Int)

data RangeChange = B Range Int
    deriving (Show)

convertGame :: Game Int Mapping -> Game Range [RangeChange]
convertGame (Game seeds' mappings') =
    Game (mkRanges seeds') $ fmap rangeChanges <$> mappings'

rangeChanges :: Mapping -> [RangeChange]
rangeChanges m = do
    ((k', x'), (k'', _x)) <- zip <*> tail $ Map.toList m
    pure $ B (k', k'' - 1) x'

consume :: Range -> [RangeChange] -> [Range]
consume (x, y) [] = [(x, y)]
consume (x, y) (B (x', y') d : xs)
    | y < x' = [(x, y)]
    | x > y' = consume (x, y) xs
    | x < x' && y <= y' = [(x, x'), (x' + d, y + d)]
    | y <= y' = [(x + d, y + d)]
    | otherwise = (x + d, y' + d) : consume (y' + 1, y) xs

run2 :: Target -> Game Range [RangeChange] -> [Range]
run2 target' (Game rs mappings') = case Map.lookup target' mappings' of
    Nothing -> rs
    Just (MappingTo to bs) -> (\rs' -> run2 to (Game rs' mappings')) $ do
        (x, y) <- rs
        consume (x, y) bs

mkRanges :: [Int] -> [Range]
mkRanges [] = []
mkRanges [_] = []
mkRanges (x : y : xs) = (x, y + x - 1) : mkRanges xs
