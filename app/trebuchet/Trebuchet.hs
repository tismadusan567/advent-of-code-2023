module Trebuchet where

import Data.Char (isDigit, digitToInt)
import Data.Foldable ( find )
import qualified Data.Maybe

sumOfCalibration :: [String] -> Int
sumOfCalibration strings = sum $ map parseString strings
    where 
        parseString str = firstNum str * 10 + firstNum (reverse str)
        firstNum str = digitToInt $ Data.Maybe.fromMaybe '0' (find isDigit str)

main :: IO ()
main = do
    s <- readFile "trebuchet\\input.txt"
    print $ sumOfCalibration (lines s)