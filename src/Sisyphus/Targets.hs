module Sisyphus.Targets
( supportedTargets
)where


import qualified Data.Map.Strict as M
import Sisyphus.Language.Graphviz (renderGvSimple)
import Sisyphus.Language.C (renderCSimple)


supportedTargets = M.fromList [ ("Graphviz_Simple", renderGvSimple)
                              , ("C_Simple", renderCSimple)]
