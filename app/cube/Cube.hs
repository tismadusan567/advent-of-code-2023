module Cube where

import Data.List.Split
import Data.Map.Strict (fromList, (!), Map)
import Data.Maybe

sumOfGameIds :: [String] -> Int
sumOfGameIds strings = sum . catMaybes $ map validGame strings

validGame :: String -> Maybe Int
validGame str = if validMoves moves then Just gameId else Nothing
    where (gameId, moves) = parseGame str

parseGame :: String -> (Int, String)
parseGame str = (gameId, moves)
    where 
        [game, moves]= splitOn ":" str
        gameId = read $ (words $ game) !! 1

parseMoves :: String -> [Map String Int]
parseMoves moves = map parseMove (splitOn ";" moves)

parseMove :: String -> Map String Int
parseMove move = fromList $ map parseEntry (splitOn "," move)
    where 
        parseEntry entry = let [num, color] = words entry in (color, read num)

validMoves :: String -> Bool
validMoves str = foldl validSet True (splitOn ";" str)
    where 
        validSet acc set = if acc then foldl validEntry True (splitOn "," set) else False
        validEntry acc entry = let [num, color] = words entry in if acc then (read num) <= (colorMap ! color) else False
        colorMap = fromList [("blue", 14), ("green", 13), ("red", 12)]

cubeMain :: IO ()
cubeMain = do
    s <- readFile "app\\cube\\input.txt"
    print $ sumOfGameIds (lines s)