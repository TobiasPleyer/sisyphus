module Main where

import qualified Data.Map as M
import System.Exit (ExitCode(..), exitWith)
import Sisyphus.Lexer (tokenize)
import Sisyphus.Parser (parse)
import Sisyphus.Types
import Sisyphus.Language (supportedTargets)


main :: IO ()
main = do
  sgf <- getContents
  let rsm = parse $ tokenize sgf
      outFile = (rsmName rsm) ++ ".gv"
      target = "Graphviz_Simple"
      maybeRenderFunc = M.lookup target supportedTargets
  case maybeRenderFunc of
    Nothing -> do
      putStrLn $ "Target '" ++ target ++ "' is not supported!"
      exitWith (ExitFailure 1)
    Just renderFunc -> do
      putStrLn $ "Rendering target '" ++ target ++ "' to file " ++ outFile ++ "..."
      renderFunc rsm outFile
      putStrLn "Done"
