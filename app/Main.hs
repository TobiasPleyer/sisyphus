module Main where

import qualified Data.Map.Strict as M
import System.Exit (ExitCode(..), exitWith)
import Sisyphus.Lexer (tokenize)
import Sisyphus.Parser (parse)
import Sisyphus.Types
import Sisyphus.Targets (supportedTargets)


tryRenderTarget target vsm outFile = do
  let maybeRenderFunc = M.lookup target supportedTargets
  case maybeRenderFunc of
    Nothing -> do
      putStrLn $ "Target '" ++ target ++ "' is not supported!"
      exitWith (ExitFailure 1)
    Just renderFunc -> do
      putStrLn $ "Rendering target '" ++ target ++ "' to file " ++ outFile ++ "..."
      renderFunc vsm outFile
      putStrLn "Done"


main :: IO ()
main = do
  sgf <- getContents
  let sm = parse $ tokenize sgf
  tryRenderTarget "Graphviz_Simple" sm ("tmp/" ++ (smName sm) ++ ".gv")
  print "Done"
