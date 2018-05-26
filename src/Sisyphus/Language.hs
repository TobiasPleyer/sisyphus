module Sisyphus.Language
( supportedTargets
)where


import qualified Data.Map as M
import Sisyphus.Language.Graphviz (renderGvSimple)


supportedTargets = M.fromList [ ("Graphviz_Simple", renderGvSimple) ]
