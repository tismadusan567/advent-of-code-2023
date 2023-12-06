module Trebuchet2 where

import Data.Char (isDigit, digitToInt)
import Data.Foldable ( find )
import Data.Maybe
import Data.List
import qualified Data.Map.Strict as Map
import Data.Map.Strict (fromList, (!), keys)

sumOfCalibration :: [String] -> Int
sumOfCalibration strings = sum $ map parseString strings

numbers :: Map.Map String Int
numbers = fromList [("one", 1),("two", 2), ("three", 3), ("four", 4), ("five", 5), ("six", 6), ("seven", 7),("eight", 8), ("nine", 9), ("1", 1), ("2", 2), ("3", 3), ("4", 4), ("5", 5), ("6", 6), ("7", 7), ("8", 8), ("9", 9), ("0", 0)]

parseString :: String -> Int
parseString str = (numbers ! firstKey) * 10 + (numbers ! secondKey)
    where
        firstKey = snd $ foldl (firstNum str) (maxBound, "") (keys numbers)
        secondKey = snd $ foldl (lastNum str) (-1, "") (keys numbers)

firstNum :: String -> (Int, String) -> String -> (Int, String)
firstNum str (accIdx, accKey) key = if idx < accIdx then (idx, key) else (accIdx, accKey)
    where idx = fromMaybe maxBound (findFirstSubtring key str)

lastNum :: String -> (Int, String) -> String -> (Int, String)
lastNum str (accIdx, accKey) key = if idx > accIdx then (idx, key) else (accIdx, accKey)
    where idx = fromMaybe (-1) (findLastSubstring key str)

findFirstSubtring :: (Eq a) => [a] -> [a] -> Maybe Int
findFirstSubtring search str = findIndex (isPrefixOf search) (tails str)

findLastSubstring :: (Eq a) => [a] -> [a] -> Maybe Int
findLastSubstring search str = findLastIndex (isPrefixOf search) (tails str)

findLastIndex :: (a -> Bool) -> [a] -> Maybe Int
findLastIndex pred str = if null indices then Nothing else Just (last indices)
    where indices = findIndices pred str

trebuchet2Main :: IO ()
trebuchet2Main = do
    s <- readFile "app\\trebuchet\\input.txt"
    print $ sumOfCalibration (lines s)