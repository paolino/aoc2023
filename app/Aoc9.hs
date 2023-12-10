{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Fuse foldr/fmap" #-}

module Aoc9 where

import Data.Attoparsec.ByteString.Char8
    ( Parser
    , char
    , decimal
    , endOfLine
    , many1
    , parseOnly
    , sepBy1
    , signed
    )
import Data.ByteString qualified as B

main :: IO (Int, Int)
main = do
    xs <- B.getContents
    let Right seqs = parseOnly parseSeqs xs
        us = untillAll1 <$> seqs
        rs1 = sum . fmap last <$> us
        rs2 = foldr (-) 0 . fmap head <$> us
    pure (sum rs1, sum rs2)

parseSeqs :: Parser [[Int]]
parseSeqs = many1 $ sepBy1 (signed decimal) (char ' ') <* endOfLine

diffs :: [Int] -> [Int]
diffs xs = zipWith (-) (tail xs) xs

untillAll1 :: [Int] -> [[Int]]
untillAll1 = takeWhile (not . all (== 0)) . iterate diffs
