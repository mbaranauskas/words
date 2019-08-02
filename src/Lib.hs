module Lib
    ( grid
    , languages
    , formatGrid
    , outputGrid
    , findWord
    , findWords
    ,findWordInLine
    , skew
    ) where

import Data.List(isInfixOf, transpose)
import Data.Maybe(catMaybes)

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
    let lines = getLines grid
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

grid = [ "__C________R___"
       , "__SI________U__"
       , "__HASKELL____B_"
       , "__A__A_____S__Y"
       , "__R___B___C____"
       , "__PHP____H_____"
       , "____S_LREP_____"
       , "____I__M_Y__L__"
       , "____L_E__T_O___"
       , "_________HB____"
       , "_________O_____"
       , "________CN_____"
       ]

languages = [ "BASIC"
            , "COBOL"
            , "CSHARP"
            , "HASKELL"
            , "LISP"
            , "PERL"
            , "PHP"
            , "PYTHON"
            , "RUBY"
            , "SCHEME"
            ]