module Sisyphus.Targets

where


import System.Exit (ExitCode(..), exitWith)
import qualified Data.Map.Strict as M
import Sisyphus.Language.Graphviz
import Sisyphus.Language.C


supportedTargets = M.fromList [ ("Graphviz", renderGraphviz)
                              , ("C", renderCSimple)
                              , ("C::Simple", renderCSimple)
                              ]


renderTarget sm outDir target = do
  let maybeRenderFunc = M.lookup target supportedTargets
  case maybeRenderFunc of
    Nothing -> putStrLn $ "Target '" ++ target ++ "' is not supported!"
    Just renderFunc -> do
      putStrLn $ "Rendering target '" ++ target
      renderFunc sm outDir
      putStrLn $ "...done rendering '" ++ target ++ "'"
