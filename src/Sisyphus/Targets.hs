module Sisyphus.Targets

where


import System.Exit (ExitCode(..), exitWith)
import qualified Data.Map.Strict as M
import Sisyphus.Language.Graphviz (renderGvSimple)
import Sisyphus.Language.C (renderCSimple)


supportedTargets = M.fromList [ ("Graphviz_Simple", renderGvSimple)
                              , ("C_Simple", renderCSimple)]


tryRenderTarget target sm outFile = do
  let maybeRenderFunc = M.lookup target supportedTargets
  case maybeRenderFunc of
    Nothing -> do
      putStrLn $ "Target '" ++ target ++ "' is not supported!"
      exitWith (ExitFailure 1)
    Just renderFunc -> do
      putStrLn $ "Rendering target '" ++ target ++ "' to file " ++ outFile ++ "..."
      renderFunc sm outFile
      putStrLn "Done"
