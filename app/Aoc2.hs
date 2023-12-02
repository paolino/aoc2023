{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Aoc2 where

import Control.Applicative ((<|>))
import Data.Attoparsec.ByteString.Char8
    ( Parser
    , decimal
    , parseOnly
    , sepBy
    , space
    , string
    )
import Data.ByteString.Char8 qualified as B
import Data.Either (rights)
import Data.Functor (($>))
import Data.Map qualified as Map
import Data.Map.Strict (Map)

main :: IO (Int, Int)
main = do
    xs <- B.lines <$> B.getContents
    let gs = rights $ parseOnly parseGame <$> xs
    let r1 = sum $ do
            Game n ms <- gs
            if all (possibleGrab limit) ms
                then pure n
                else []
    let r2 = sum $ do
            Game _n ms <- gs
            let m = Map.unionsWith max ms
            pure $ product m
    pure (r1, r2)

limit :: Grab
limit = Map.fromList [(Red, 12), (Green, 13), (Blue, 14)]

data Color = Red | Green | Blue
    deriving (Show, Eq, Ord)

type Grab = Map Color Int

possibleGrab :: Grab -> Grab -> Bool
possibleGrab mp m = and $ do
    (c, n) <- Map.toList m
    pure $ case Map.lookup c mp of
        Nothing -> False
        Just m' -> m' >= n

data Game = Game Int [Grab]
    deriving (Show)

parseGame :: Parser Game
parseGame = do
    string "Game"
    space
    n <- decimal @Int
    string ":"
    Game n <$> sepBy parseGrab ";"

parseGrab :: Parser Grab
parseGrab = do
    xs <- sepBy parseColor ","
    pure $ Map.fromList xs

parseColor :: Parser (Color, Int)
parseColor = do
    space
    n <- decimal @Int
    space
    c <- parseColor'
    pure (c, n)

parseColor' :: Parser Color
parseColor' =
    string "red" $> Red
        <|> string "green" $> Green
        <|> string "blue" $> Blue
