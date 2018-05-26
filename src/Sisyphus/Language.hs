module Sisyphus.Language
( supportedTargets
)where


import qualified Data.Map as M
import Sisyphus.Language.Graphviz (renderGvSimple)
import Sisyphus.Language.C (renderCSimple)


supportedTargets = M.fromList [ ("Graphviz_Simple", renderGvSimple)
                              , ("C_Simple", renderCSimple)]
