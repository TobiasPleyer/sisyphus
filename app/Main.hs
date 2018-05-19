module Main where

import Sisyphus.Lexer (tokenize)
import Sisyphus.Parser (parse)

main :: IO ()
main = getContents >>= print . parse . tokenize
