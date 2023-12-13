{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Aoc12 where

import Control.Applicative ((<|>))
import Control.Monad (guard)
import Control.Monad.State
    ( MonadPlus (..)
    , MonadTrans (..)
    , State
    , evalState
    , gets
    , modify
    )
import Data.Attoparsec.ByteString.Char8
    ( Parser
    , char
    , decimal
    , endOfLine
    , many1
    , parseOnly
    , sepBy
    , space
    )
import Data.ByteString qualified as B
import Data.Functor (($>))
import Data.List (intercalate)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Monoid (Sum (..))
import ListT (ListT, toList)

main :: IO (Int, Int)
main = do
    xs <- B.getContents
    let Right fields = parseOnly parseFields xs
    pure (sum $ solve <$> fields, sum $ solve . fatField 5 <$> fields)

data Spring = Damaged | Operational | Unknown
    deriving (Show, Eq, Ord)

data Field = Field
    { springs :: [Spring]
    , folded :: [Int]
    }
    deriving (Show, Eq, Ord)

parseFields :: Parser [Field]
parseFields = many1 (parseField <* endOfLine)

parseField :: Parser Field
parseField = do
    springs <- many1 parseSpring
    space
    folded <- decimal `sepBy` char ','
    pure $ Field{..}

parseSpring :: Parser Spring
parseSpring =
    (char '#' $> Damaged)
        <|> (char '.' $> Operational)
        <|> (char '?' $> Unknown)

minLength :: [Int] -> Int
minLength xs = sum xs + length xs - 1

type Springs = [Spring]

solve :: Field -> Int
solve (Field{..}) =
    getSum . mconcat
        $ flip evalState mempty
        $ toList
        $ search (length springs, minLength folded)
        $ Key springs Nothing folded

type Cut = (Int, Int)

cutting :: (Int, Int) -> Bool
cutting (l, r) = l >= r

downBoth :: Cut -> Cut
downBoth (l, r) = (l - 1, r - 1)

downFst :: Cut -> Cut
downFst (l, r) = (l - 1, r)

data Key = Key {tbd :: Springs, acc :: Maybe Int, missing :: [Int]}
    deriving (Show, Eq, Ord)

type Cache k r = State (Map k r)

search :: (Int, Int) -> Key -> ListT (Cache Key (Sum Int)) (Sum Int)
-- search (cutting -> False) _ = mzero
search cut k@Key{..} = caching k $ dive tbd acc missing
  where
    again cut' tbd' acc' missing' = search cut' (Key tbd' acc' missing')
    dive [] _ [] = pure $ Sum 1
    dive [] (Just l') [l] = do
        guard $ l == l'
        pure $ Sum 1
    dive (Damaged : _) Nothing [] = mzero
    dive (Damaged : xs) Nothing ls = again (downBoth cut) xs (Just 1) ls
    dive (Damaged : xs) (Just l') ls@(l : _) = do
        guard $ l' + 1 <= l
        again (downBoth cut) xs (Just $ l' + 1) ls
    dive (Operational : xs) Nothing ls = again (downFst cut) xs Nothing ls
    dive (Operational : xs) (Just l') (l : ls)
        | l == l' = again (downBoth cut) xs Nothing ls
        | otherwise = mzero
    dive (Unknown : xs) c ls =
        again cut (Damaged : xs) c ls <> again cut (Operational : xs) c ls
    dive _ _ _ = mzero

caching
    :: (Ord k, Monoid r)
    => k
    -> ListT (State (Map k r)) r
    -> ListT (State (Map k r)) r
caching k r = do
    cache <- gets $ Map.lookup k
    case cache of
        Just c -> pure c
        Nothing -> do
            c <- mconcat <$> lift (toList r)
            modify $ Map.insert k c
            pure c

fatField :: Int -> Field -> Field
fatField n Field{..} =
    Field
        (intercalate [Unknown] $ replicate n springs)
        (concat $ replicate n folded)
