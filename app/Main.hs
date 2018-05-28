module Main where

import qualified Data.Map.Strict as M
import System.Exit (ExitCode(..), exitWith)
import Sisyphus.Lexer (tokenize)
import Sisyphus.Parser (parse)
import Sisyphus.Types
import Sisyphus.Language (supportedTargets)


tryRenderTarget target rsm outFile = do
  let maybeRenderFunc = M.lookup target supportedTargets
  case maybeRenderFunc of
    Nothing -> do
      putStrLn $ "Target '" ++ target ++ "' is not supported!"
      exitWith (ExitFailure 1)
    Just renderFunc -> do
      putStrLn $ "Rendering target '" ++ target ++ "' to file " ++ outFile ++ "..."
      renderFunc rsm outFile
      putStrLn "Done"


main :: IO ()
main = do
  sgf <- getContents
  let rsm = parse $ tokenize sgf
      fsm_name = rsmName rsm
  tryRenderTarget "Graphviz_Simple" rsm ("tmp/" ++ fsm_name ++ ".gv")
  tryRenderTarget "C_Simple" rsm ("tmp/" ++ fsm_name ++ ".h")
