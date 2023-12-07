{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Aoc7 where

import Control.Applicative (asum)
import Data.Attoparsec.ByteString.Char8
    ( Parser
    , char
    , decimal
    , digit
    , endOfLine
    , many1
    , parseOnly
    , space
    )
import Data.ByteString qualified as B
import Data.Char (digitToInt)
import Data.List (group, maximumBy, sort, sortBy)
import Data.Ord (comparing)

main :: IO (Int, Int)
main = do
    xs <- B.getContents
    case parseOnly parseHands xs of
        Left e -> error e
        Right cs -> do
            let rs = sortBy handOrdering cs
                rs' = sortBy handOrdering' $ fmap jcards <$> rs
            -- pPrint $ maximumBy (comparing $ figureOf . cards) . unfold <$> css
            pure (value rs, value rs')

newtype Card = Card {rank :: Int}
    deriving (Show, Eq, Ord)

data Hand cs = Hand {handCards :: cs, bet :: Int}
    deriving (Show, Functor)

type HandOfCards = Hand [Card]

parseHands :: Parser [HandOfCards]
parseHands = many1 $ parseHand <* endOfLine

parseHand :: Parser HandOfCards
parseHand = do
    cards <- many1 parseCard <* space
    Hand cards <$> decimal

parseCard :: Parser Card
parseCard = asum [Card <$> parseNumber, Card <$> parseFace]

parseNumber :: Parser Int
parseNumber = digitToInt <$> digit

parseFace :: Parser Int
parseFace =
    asum
        [ 10 <$ char 'T'
        , 11 <$ char 'J'
        , 12 <$ char 'Q'
        , 13 <$ char 'K'
        , 14 <$ char 'A'
        ]

data Figure
    = HighCard
    | Pair
    | TwoPairs
    | ThreeOfAKind
    | FullHouse
    | FourOfAKind
    | FiveOfAKind
    deriving (Show, Eq, Ord)

figureOf :: [Card] -> Figure
figureOf cs = case sgs of
    [5] -> FiveOfAKind
    [1, 4] -> FourOfAKind
    [2, 3] -> FullHouse
    [1, 1, 3] -> ThreeOfAKind
    [1, 2, 2] -> TwoPairs
    [1, 1, 1, 2] -> Pair
    [1, 1, 1, 1, 1] -> HighCard
    _ -> error "bullshit"
  where
    sgs = sort $ length <$> group (sort cs)

--------------------------------------------------------------------------------
-- part 1
--------------------------------------------------------------------------------

handOrdering :: HandOfCards -> HandOfCards -> Ordering
handOrdering = comparing (figureOf . handCards) <> comparing handCards

value :: [Hand cs] -> Int
value = sum . zipWith (*) [1 ..] . fmap bet

--------------------------------------------------------------------------------
-- part 2
--------------------------------------------------------------------------------

data JCard = Jolly Int | JCard Card
    deriving (Show, Eq, Ord)

cardsWithJolly :: [JCard] -> [Card]
cardsWithJolly = fmap $ \case
    Jolly j -> Card j
    JCard c -> c

unfold :: [Card] -> [[JCard]]
unfold [] = [[]]
unfold (c : cs) = do
    ds <- unfold cs
    case c of
        Card 11 -> do
            j <- [2 .. 10] <> [12 .. 14]
            pure $ Jolly j : ds
        _ -> pure $ JCard c : ds

jcards :: [Card] -> [JCard]
jcards = maximumBy (comparing $ figureOf . cardsWithJolly) . unfold

cardsNoJolly :: [JCard] -> [Card]
cardsNoJolly = fmap $ \case
    Jolly _ -> Card 1
    JCard c -> c

handOrdering' :: Hand [JCard] -> Hand [JCard] -> Ordering
handOrdering' =
    comparing (figureOf . cardsWithJolly . handCards)
        <> comparing (cardsNoJolly . handCards)