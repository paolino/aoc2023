{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Aoc15 where

import Control.Applicative (Alternative (..))
import Control.Monad (guard)
import Data.Attoparsec.ByteString.Char8
    ( Parser
    , char
    , decimal
    , many1
    , parseOnly
    , satisfy
    , sepBy1
    )
import Data.ByteString qualified as B
import Data.Char (isAlpha, ord)
import Data.Foldable (foldl')
import Data.Functor (($>))
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)

main :: IO (Int, Int)
main = do
    xs <- B.getContents
    let Right gs = parseOnly parseInput xs
        r1 = sum $ fmap hashing gs
    let Right hs = parseOnly parseLabels xs
        r2 = power $ foldl' apply Map.empty hs
    pure (r1, r2)

parseInput :: Parser [String]
parseInput = sepBy1 (many1 letter) (char ',')

letter :: Parser Char
letter = satisfy (\x -> x /= '\n' && x /= ',')

hashing :: String -> Int
hashing = foldl' f 0
  where
    f :: Int -> Char -> Int
    f acc c = (17 * (acc + ord c)) `mod` 256

type Focal = Int

data Operation = Equal Focal | Dash
    deriving (Show)

type Label = String

type Box = Int

data Change = Change
    { box :: Box
    , label :: Label
    , op :: Operation
    }
    deriving (Show)

data Lens = Lens
    { lensLabel :: Label
    , focal :: Focal
    }
    deriving (Show)

parseLabels :: Parser [Change]
parseLabels = sepBy1 parseChange (char ',')

parseChange :: Parser Change
parseChange = do
    label <- parseLabel
    let box = hashing label
    op <- parseOperation
    pure $ Change{..}

parseLabel :: Parser Label
parseLabel = many1 (satisfy isAlpha)

parseOperation :: Parser Operation
parseOperation = Equal <$> (char '=' *> decimal) <|> (char '-' $> Dash)

type Game = Map Box [Lens]

removeLens :: [Lens] -> Label -> [Lens]
removeLens [] _ = []
removeLens (x : xs) y =
    if lensLabel x == y
        then xs
        else x : removeLens xs y

replaceLens :: [Lens] -> Lens -> Maybe [Lens]
replaceLens [] _ = Nothing
replaceLens (x : xs) y =
    if lensLabel x == lensLabel y
        then Just (y : xs)
        else (x :) <$> replaceLens xs y

apply :: Game -> Change -> Game
apply g (Change box lens op) = case op of
    Equal x ->
        let
            l = Lens lens x
            i y = Map.insert box y g
         in
            i $ case Map.lookup box g of
                Nothing -> [l]
                Just xs -> fromMaybe (l : xs) (replaceLens xs l)
    Dash ->
        let f mxs = do
                xs <- mxs
                let ys = removeLens xs lens
                guard $ not $ null ys
                pure ys
         in Map.alter f box g

power :: Game -> Int
power g = sum $ do
    (bn, xs) <- Map.toList g
    (k, x) <- zip [1 ..] $ reverse xs
    pure $ (1 + bn) * k * focal x
