{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Aoc4 where

import Data.Attoparsec.ByteString
    ( Parser
    , many1
    , parseOnly
    , sepBy
    , string
    )
import Data.Attoparsec.ByteString.Char8
    ( decimal
    , space
    )
import Data.ByteString.Char8 qualified as B
import Data.Foldable (foldl')
import Data.Functor (($>))
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set qualified as Set

main :: IO (Int, Int)
main = do
    xs <- B.getContents
    let Right cards = parseOnly parseCards xs
    pure (solve1 cards, solve2 cards)

data Card = Card
    { number :: Int
    , value :: Int
    }
    deriving (Show)

--------------------------------------------------------------------------------
-- Parsing
--------------------------------------------------------------------------------

spaces :: Parser ()
spaces = many1 space $> ()

parseCard :: Parser Card
parseCard = do
    string "Card" <* spaces
    n <- decimal
    string ":" <* spaces
    xs <- decimal @Int `sepBy` spaces
    spaces *> string "|" *> spaces
    ys <- decimal `sepBy` spaces
    pure
        $ Card n
        $ length
        $ Set.intersection (Set.fromList xs) (Set.fromList ys)

parseCards :: Parser [Card]
parseCards = sepBy parseCard space

--------------------------------------------------------------------------------
-- Part 1
--------------------------------------------------------------------------------

evalCard1 :: Card -> Int
evalCard1 (Card _n m) = 2 ^ max 0 (m - 1)

solve1 :: [Card] -> Int
solve1 = sum . fmap evalCard1

--------------------------------------------------------------------------------
-- Part 2
--------------------------------------------------------------------------------

type Count = Map Int Int

applyCard :: Count -> Card -> Count
applyCard p (Card n v) = foldl' updatePower p [n + 1 .. n + v]
  where
    updatePower cs k = Map.adjust (+ p Map.! n) k cs

applyAllCards :: [Card] -> Count
applyAllCards cards = foldl'
    do applyCard
    do Map.fromList [(n, 1) | Card n _ <- cards]
    do cards

solve2 :: [Card] -> Int
solve2 = sum . applyAllCards