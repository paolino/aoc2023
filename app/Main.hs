module Main where

import Aoc1 qualified
import Aoc2 qualified
import Aoc3 qualified
import Aoc4 qualified
import Aoc5 qualified
import System.Environment (getArgs)

main :: IO ()
main = do
    (x : _) <- getArgs
    case read x of
        (1 :: Int) -> Aoc1.main >>= print
        2 -> Aoc2.main >>= print
        3 -> Aoc3.main >>= print
        4 -> Aoc4.main >>= print
        5 -> Aoc5.main >>= print
        _ -> error "unknown day"