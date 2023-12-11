module Main where

import Aoc1 qualified
import Aoc2 qualified
import Aoc3 qualified
import Aoc4 qualified
import Aoc5 qualified
import Aoc6 qualified
import Aoc7 qualified
import Aoc9 qualified
import Aoc10 qualified
import Aoc11 qualified
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
        6 -> Aoc6.main >>= print
        7 -> Aoc7.main >>= print
        9 -> Aoc9.main >>= print
        10 -> Aoc10.main >>= print
        11 -> Aoc11.main >>= print
        _ -> error "unknown day"