{-# LANGUAGE RecordWildCards #-}

module Sisyphus.Compile
( checkDefinitionCompleteness
)where


import Data.Maybe (fromJust)
import Data.Either (isRight)
import Control.Monad.Trans.Writer.Lazy
import qualified Data.Map.Strict as M
import qualified Data.IntMap.Strict as IM
import qualified Data.Set as S
import Sisyphus.Types
import Sisyphus.Util (addIngoingTransition, addOutgoingTransition)


checkDefinitionCompleteness :: StateMachine -> GrammarSummary
checkDefinitionCompleteness sm = undefined --foldr check initialSummary smgTransitions
--  where
--    initialSummary = GS
--                      sm
--                      (S.fromList smEvents)
--                      (S.fromList smActions)
--                      (S.fromList (M.keys smStates))
--                      (S.fromList (M.keys smStates))
--                      [] []
--    check t@(TSpec src dst trig guards reactions) GrammarSummary{..} =
--      let
--        sm@(SM name events actions states ts)
--        (events',eventsUnused') = case lookup trig events
--      in
--        GS 
