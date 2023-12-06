module Cube where

import Data.List.Split
import Data.Map.Strict (fromList, (!))
import Data.Maybe

sumOfGameIds :: [String] -> Int
sumOfGameIds strings = sum . catMaybes $ map parseGame strings

parseGame :: String -> Maybe Int
parseGame str = if valid cubes then Just gameId else Nothing
    where 
        [game, cubes]= splitOn ":" str
        gameId = read $ (words $ game) !! 1

valid :: String -> Bool
valid str = foldl validSet True (splitOn ";" str)
    where 
        validSet acc set = if acc then foldl validEntry True (splitOn "," set) else False
        validEntry acc entry = let [num, color] = words entry in if acc then (read num) <= (colorMap ! color) else False
        colorMap = fromList [("blue", 14), ("green", 13), ("red", 12)]

cubeMain :: IO ()
cubeMain = do
    s <- readFile "app\\cube\\input.txt"
    print $ sumOfGameIds (lines s)