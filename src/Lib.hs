module Lib
    ( languages
    , formatGrid
    , outputGrid
    , findWord
    , findWords
    ,findWordInLine
    , skew
    ) where

import Data.List(isInfixOf, transpose)
import Data.Maybe(catMaybes)
import Data

type Grid = [String]

formatGrid :: Grid -> String
formatGrid = unlines

outputGrid :: Grid -> IO ()
outputGrid gr = putStrLn $formatGrid gr

getLines :: Grid -> [String]
getLines grid = (getReversedLines grid) ++ (getReversedLines $ transpose grid)

getReversedLines :: Grid -> [String]
getReversedLines grid = grid ++ (map reverse grid)

findWord :: Grid -> String -> Maybe String
findWord grid word = 
    let lines = (getLines grid) ++ (getLines $ skew grid) ++ (getLines$ skew (reverse grid))
        found =  or $ map(findWordInLine word) lines
    in if found then Just word else Nothing

findWords :: Grid -> [String] -> [String]
findWords grid words = catMaybes $ map (findWord grid) words

findWordInLine :: String -> String -> Bool
findWordInLine = isInfixOf

skew :: Grid -> Grid
skew [] = []
skew (l:ls) = l : skew (map indent ls)
    where indent line = '_' : line