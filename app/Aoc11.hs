{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Aoc11 where

import Control.Applicative (Alternative (..))
import Data.Attoparsec.ByteString.Char8
    ( Parser
    , char
    , endOfLine
    , many1
    , parseOnly
    )
import Data.ByteString qualified as B
import Data.Functor (($>))
import Data.List (transpose, tails)
import Data.Map (Map)
import qualified Data.Map as Map

main :: IO (Int, Int)
main = do
    xs <- B.getContents
    let Right space = parseOnly parseSpace xs
        spaceN n = expandCols n $ expandRows n space
        gs = galaxies space
        r n = sum $ distances (mkBoard $ spaceN n) gs
    pure (r 2, r 1000000)

data Space = Empty Int | Galaxy
    deriving (Show, Eq)

parseSpace :: Parser [[Space]]
parseSpace = many1 $ parseLine <* endOfLine

parseLine :: Parser [Space]
parseLine = many1 $ (char '.' $> Empty 1) <|> (char '#' $> Galaxy)

expandRows :: Int -> [[Space]] -> [[Space]]
expandRows n = foldr duplicateEmptyRow []
    where
        duplicateEmptyRow :: [Space] -> [[Space]] -> [[Space]]
        duplicateEmptyRow [] acc = [] : acc
        duplicateEmptyRow x acc
            | allEmpty x = fmap (\(Empty m) -> Empty $ n * m) x : acc
            | otherwise = x : acc

allEmpty :: [Space] -> Bool
allEmpty = all f
    where f (Empty _) = True
          f _ = False

expandCols :: Int -> [[Space]] -> [[Space]]
expandCols n = transpose . expandRows n . transpose

data Coo = Coo Int Int
    deriving (Show, Eq, Ord)

type Board = Map Coo Space

galaxies :: [[Space]] -> [Coo]
galaxies xss = do
    (y , xs) <- zip [0..] xss
    (x, Galaxy) <- zip [0..] xs
    pure $ Coo x y

mkBoard :: [[Space]] -> Board
mkBoard xss = Map.fromList $ do
    (y, xs) <- zip [0..] xss
    (x, s) <- zip [0..] xs
    pure (Coo x y, s)

path :: Coo -> Coo -> [Coo]
path c1 c2 = tail $ go c1 c2
    where
        go :: Coo -> Coo -> [Coo]
        go c@(Coo x1  y1) (Coo x2 y2) = case compare x1 x2 of
                LT -> c : go (Coo (x1 + 1) y1) c2
                GT -> c : go (Coo (x1 - 1) y1) c2
                EQ -> c : case compare y1 y2 of
                    LT -> go (Coo x1 (y1 + 1)) c2
                    GT -> go (Coo x1 (y1 - 1)) c2
                    EQ -> []

distance :: Board -> [Coo] -> Int
distance b xs = sum $ fmap distanceFrom xs
    where
        distanceFrom :: Coo -> Int
        distanceFrom c = case b Map.! c of
            Empty n -> n
            Galaxy -> 1

distances :: Board -> [Coo] -> [Int]
distances b cs = do
    c1 : cs' <- tails cs
    distance b . path c1 <$> cs'