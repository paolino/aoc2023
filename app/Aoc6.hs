{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Aoc6 where

import Control.Monad (void)
import Data.Attoparsec.ByteString
    ( Parser
    , many1
    , parseOnly
    , sepBy
    , string
    )
import Data.Attoparsec.ByteString.Char8 (decimal, digit, space)
import Data.ByteString qualified as B

main :: IO (Int, Int)
main = do
    xs <- B.getContents
    let Right (times, distances) = parseOnly (parseInput decimal) xs
        r1 = product $ uncurry range <$> zip times distances
    let Right (ts, ds) = parseOnly (parseInput $ many1 digit) xs
        r2 = range (read $ concat ts) (read $ concat ds)
    return (r1, r2)

parseInput :: Parser a -> Parser ([a], [a])
parseInput p = do
    string "Time:" <* spaces
    ts <- sepBy p spaces
    spaces *> string "Distance:" <* spaces
    ds <- sepBy p spaces
    return (ts, ds)
  where
    spaces :: Parser ()
    spaces = void $ many1 space

root :: Int -> Int -> Int
root t d = floor $ (kf + sqrt (kf * kf - 4 * yf)) / 2
  where
    kf = fromIntegral t :: Double
    yf = fromIntegral d

range :: Int -> Int -> Int
range t d = 2 * root t d - t + 1