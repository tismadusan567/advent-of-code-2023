module Cube where

import Data.List.Split
import Data.Map.Strict (fromList, (!), Map, keys)
import Data.Maybe

sumOfGameIds :: [String] -> Int
sumOfGameIds strings = sum . catMaybes $ map validGame strings

validGame :: String -> Maybe Int
validGame str = if validMoves moves then Just gameId else Nothing
    where (gameId, moves) = parseGame str

parseGame :: String -> (Int, [Map String Int])
parseGame str = (gameId, parseMoves moves)
    where 
        [game, moves]= splitOn ":" str
        gameId = read $ (words $ game) !! 1

parseMoves :: String -> [Map String Int]
parseMoves moves = map parseMove (splitOn ";" moves)

parseMove :: String -> Map String Int
parseMove move = fromList $ map parseEntry (splitOn "," move)
    where 
        parseEntry entry = let [num, color] = words entry in (color, read num)

validMoves :: [Map String Int] -> Bool
validMoves moves = foldl validMove True moves
    where 
        validMove acc move = if acc then foldl (validColor move) True (keys move) else False
        validColor move acc color = if acc then move ! color <= colorMap ! color else False
        colorMap = fromList [("blue", 14), ("green", 13), ("red", 12)]

cubeMain :: IO ()
cubeMain = do
    s <- readFile "app\\cube\\input.txt"
    print $ sumOfGameIds (lines s)