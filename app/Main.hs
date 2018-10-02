module Main where

import Control.Monad (forM_)

import Sisyphus.Lexer (tokenize)
import Sisyphus.Parser (parse)
import Sisyphus.Types
import Sisyphus.Compile
import Sisyphus.Targets (supportedTargets, tryRenderTarget)


main :: IO ()
main = do
  sgf <- getContents
  let sm = parse $ tokenize sgf
  tryRenderTarget "Graphviz_Simple" sm ("tmp/" ++ (smName sm) ++ ".gv")
  let gs = runChecks sm
  putStrLn "============================"
  putStrLn "Warnings:"
  forM_ (warnings gs) putStrLn
  putStrLn "============================"
  putStrLn "Errors:"
  forM_ (errors gs) putStrLn
  print "Done"
