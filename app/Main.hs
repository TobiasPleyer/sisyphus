module Main where

import Control.Monad (forM_, unless)

import Sisyphus.Lexer (tokenize)
import Sisyphus.Parser (parse)
import Sisyphus.Types
import Sisyphus.Compile
import Sisyphus.Targets (supportedTargets, tryRenderTarget)


main :: IO ()
main = do
  sgf <- getContents
  let statemachine = parse $ tokenize sgf
  tryRenderTarget "Graphviz_Simple" statemachine ("tmp/" ++ (smName statemachine) ++ ".gv")
  let summary = runChecks statemachine
  unless (null (warnings summary)) $ do
    putStrLn "== Warnings =="
    forM_ (warnings summary) putStrLn
  unless (null (errors summary)) $ do
    putStrLn "== Errors =="
    forM_ (errors summary) putStrLn
  print "Done"
