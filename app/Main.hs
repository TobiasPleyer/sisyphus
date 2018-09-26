module Main where

import Sisyphus.Lexer (tokenize)
import Sisyphus.Parser (parse)
import Sisyphus.Types
import Sisyphus.Targets (supportedTargets, tryRenderTarget)


main :: IO ()
main = do
  sgf <- getContents
  let sm = parse $ tokenize sgf
  tryRenderTarget "Graphviz_Simple" sm ("tmp/" ++ (smName sm) ++ ".gv")
  print "Done"
