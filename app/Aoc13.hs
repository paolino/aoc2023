{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Aoc13 where

import Control.Applicative (Alternative (..))
import Data.Attoparsec.ByteString.Char8
    ( Parser
    , char
    , endOfLine
    , many1
    , parseOnly
    , sepBy1

    )
import Data.ByteString.Char8 qualified as B
import Data.Functor (($>))
import Data.List (find, transpose)
import Data.Maybe (mapMaybe)

main :: IO (Int, Int)
main = do
    xs <- B.getContents
    let Right gs = parseOnly parseGame xs
        rs1 = count <$> mapMaybe (anySimmetry isGood) gs
        rs2 = count <$> mapMaybe (anySimmetry $ isAlmostGood differences) gs
    pure (sum rs1, sum rs2)

data W = Ash | Mirror
    deriving (Show, Eq, Ord)

type Game = [[[W]]]

parseGame :: Parser Game
parseGame = flip sepBy1 (endOfLine *> endOfLine) $ sepBy1
    do many1 parseW
    do endOfLine

parseW :: Parser W
parseW = (char '.' $> Ash) <|> (char '#' $> Mirror)

simmetries :: [a] -> [([a], [a])]
simmetries [] = []
simmetries (y : ys) = go [y] ys
  where
    go acc [x] = [(acc, [x])]
    go acc (x : xs) = (acc, x : xs) : go (x : acc) xs
    go _ _ = error "impossible"

isGood :: (Eq b) => ([b], [b]) -> Bool
isGood (xs, ys) = and $ zipWith (==) xs ys

anySimmetry
    :: (Eq a)
    => (([[a]], [[a]]) -> Bool)
    -> [[a]]
    -> Maybe (Either ([[a]], [[a]]) ([[a]], [[a]]))
anySimmetry good xs = f Left xs <|> f Right (transpose xs)
  where
    f k = fmap k . find good . simmetries

differences :: (Eq a) => [a] -> [a] -> Int
differences xs ys = length $ filter id $ zipWith (/=) xs ys

count :: Either ([[a]], [[a]]) ([[a]], [[a]]) -> Int
count (Left (xs, _ys)) = 100 * length xs
count (Right (xs, _ys)) = length xs

isAlmostGood :: (b -> b -> Int) -> ([b], [b]) -> Bool
isAlmostGood f (xs, ys) = sum (zipWith f xs ys) == 1