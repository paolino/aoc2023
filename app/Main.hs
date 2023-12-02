{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import System.Environment (getArgs)
import qualified Aoc1
import qualified Aoc2

main :: IO ()
main = do
    (x:_ ) <- getArgs
    case read x of
        (1 :: Int) -> Aoc1.main >>= print
        2 -> Aoc2.main >>= print
        _ -> error "unknown day"