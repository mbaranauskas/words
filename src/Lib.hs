module Lib
    ( grid
    , languages
    , formatGrid
    , outputGrid
    , findWord
    ,findWordInLine
    ) where

import Data.List

type Grid = [String]

formatGrid :: Grid -> String
formatGrid = unlines

outputGrid :: Grid -> IO ()
outputGrid gr = putStrLn $formatGrid gr

findWord :: Grid -> String -> Bool
findWord = undefined

findWordInLine :: String -> String -> Bool
findWordInLine = isInfixOf

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
       , "_________CN____"
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