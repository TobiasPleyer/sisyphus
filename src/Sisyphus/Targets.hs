module Sisyphus.Targets

where


import System.Exit (ExitCode(..), exitWith)
import qualified Data.Map.Strict as M
import Sisyphus.Language.PlantUML
import Sisyphus.Language.C


supportedTargets = M.fromList [ ("PlantUML", renderPlantUML)
                              , ("C", renderCSimple)
                              , ("C::Simple", renderCSimple)
                              ]


renderTarget templateLoader sm outDir target = do
  let maybeRenderFunc = M.lookup target supportedTargets
  case maybeRenderFunc of
    Nothing -> putStrLn $ "Target '" ++ target ++ "' is not supported!"
    Just renderFunc -> do
      putStrLn $ "Rendering target '" ++ target
      renderFunc templateLoader sm outDir
      putStrLn $ "...done rendering '" ++ target ++ "'"
