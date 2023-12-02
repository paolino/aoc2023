module Aoc1 where

import Control.Monad (guard)
import Data.Bifunctor (first)
import Data.Foldable (asum)
import Data.List (isPrefixOf, tails)

main :: IO (Int, Int)
main = do
    xs <- lines <$> getContents
    let r qs = sum $ do
            x <- xs
            Just n <- pure $ findMatch qs x
            Just m <- pure $ findMatch (reversed qs) $ reverse x
            return $ 10 * n + m
        complex = numbers <> letters
    pure (r numbers, r complex)

type S = (String, Int)

match :: S -> String -> Maybe Int
match (x, n) y = do
    guard $ x `isPrefixOf` y
    pure n

findMatch :: [S] -> String -> Maybe Int
findMatch ts x = asum $ do
    y <- tails x
    t <- ts
    pure $ match t y

numbers :: [S]
numbers =
    [ ("1", 1)
    , ("2", 2)
    , ("3", 3)
    , ("4", 4)
    , ("5", 5)
    , ("6", 6)
    , ("7", 7)
    , ("8", 8)
    , ("9", 9)
    ]

letters :: [S]
letters =
    [ ("one", 1)
    , ("two", 2)
    , ("three", 3)
    , ("four", 4)
    , ("five", 5)
    , ("six", 6)
    , ("seven", 7)
    , ("eight", 8)
    , ("nine", 9)
    ]

reversed :: [S] -> [S]
reversed = fmap (first reverse)
