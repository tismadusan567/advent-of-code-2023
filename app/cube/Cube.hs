module Cube where

import Data.List.Split
import Data.Map.Strict (fromList, (!), Map, keys, insertWith)
import Data.Maybe

-- part one

sumOfGameIds :: [String] -> Int
sumOfGameIds games = sum . catMaybes $ map validGame games

validGame :: String -> Maybe Int
validGame str = if validMoves moves then Just gameId else Nothing
    where (gameId, moves) = parseGame str

validMoves :: [Map String Int] -> Bool
validMoves moves = foldl validMove True moves
    where 
        validMove acc move = if acc then foldl (validColor move) True (keys move) else False
        validColor move acc color = if acc then move ! color <= colorMap ! color else False
        colorMap = fromList [("blue", 14), ("green", 13), ("red", 12)]

-- part two

sumOfPowers :: [String] -> Int
sumOfPowers games = sum $ map gamePower games

gamePower :: String -> Int
gamePower game = foldl (*) 1 $ foldl computeMove (fromList [("blue", 0), ("red", 0), ("green", 0)]) moves
    where 
        (_, moves) = parseGame game
        computeMove acc move = foldl (\acc2 key -> insertWith max key (move ! key) acc2) acc (keys move)

-- helpers

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

cubeMain :: IO ()
cubeMain = do
    s <- readFile "app\\cube\\input.txt"
    print $ sumOfGameIds (lines s)
    print $ sumOfPowers (lines s)