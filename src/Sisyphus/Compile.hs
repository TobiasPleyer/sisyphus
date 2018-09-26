{-# LANGUAGE RecordWildCards #-}

module Sisyphus.Compile where


import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Sisyphus.Types
import Sisyphus.Util (addIngoingTransition, addOutgoingTransition)


checkDefinitionCompleteness :: StateMachine -> GrammarSummary
checkDefinitionCompleteness sm@(SM name events actions states transitions) =
  foldr check initialSummary transitions
  where
    initialSummary = GS
                      sm
                      (S.fromList events)
                      (S.fromList actions)
                      (S.fromList $ M.keys states)
                      (S.fromList $ M.keys states)
                      [] []


check :: TransitionSpec -> GrammarSummary -> GrammarSummary
check t@(TSpec src dst trig guards reactions) gs@GS{..} = gs
