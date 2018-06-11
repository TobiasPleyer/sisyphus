module Sisyphus.Compile
( mkTransitionFunctions
)where


import Data.Maybe (fromJust)
import Data.Either (isRight)
import Control.Monad.Trans.Writer.Lazy
import qualified Data.Map.Strict as M
import Sisyphus.Types
import Sisyphus.Util (addIngoingTransition, addOutgoingTransition)


validateRSM :: RawStateMachine -> M ValidatedStateMachine
validateRSM = undefined


type ErrorMsg = String
type WarningMsg = String
type M a = Writer ([ErrorMsg],[WarningMsg]) a

addError :: ErrorMsg -> M ()
addError e = tell ([e],[])

addWarning :: WarningMsg -> M ()
addWarning w = tell ([],[w])


--walkTransitions :: RawStateMachine -> M ValidatedStateMachine
--walkTransitions rsm@(RSM name es as ss ts) = go sm ts
--  where stateNames = map stName ss
--        sm = M.fromList (zip stateNames ss)
--        go sm [] = VSM name es as (map ((M.!) sm) stateNames) ts
--        go sm (tran@(TSpec src dst _ _ _):ts') =
--          let sm'  = M.update (Just . addOutgoingTransition tran) src sm
--              sm'' = M.update (Just . addIngoingTransition  tran) dst sm'
--          in go sm'' ts'


-- | This function is the central point of the implementation.
-- It will create the transition table entries by combining entry, internal and
-- exit reactions with the reactions of the transition leading to the state
-- change.
--
-- The return value of this function is a map which maps a tuple
-- (state name, event name) to a function specification.
--
-- The function specification is the blueprint of the transition function that
-- performs all the actions in the correct order, which means:
--
-- * All the exit reactions of the source state
--
-- * All reactions of the transition causing the state change
--
-- * All entry reactions of the target state
--
-- * Internal transitions are handled as a self transition of the state without
-- entry or exit reactions and will be added to the map by another function.
--
-- The transition functions will be assigned a unique name of the form
-- /from_{state name}_on_{event name}/. If this name appears more than once
-- this is treated as an ambiguity exception.
mkTransitionFunctions :: ValidatedStateMachine -> [(State,State,[Reaction])]
mkTransitionFunctions vsm = undefined
--mkTransitionFunctions vsm = addTransitions M.empty stateMap transs
--  where states = rsmStates rsm
--        transs = rsmTransitions rsm
--        stateMap = M.fromList (zip (map stName states) states)
--        addTransitions m sMap [] = addInternalReactions m states
--        addTransitions m sMap (t@(TSpec tSrc tDst Nothing _gs tReacts):ts) = Left (SmNoTriggerError (Right t))
--        addTransitions m sMap (t@(TSpec tSrc tDst (Just tTrigg) _gs tReacts):ts) =
--          let srcState = (M.!) sMap tSrc
--              dstState = (M.!) sMap tDst
--              tFuncName = "from_" ++ tSrc ++ "_on_" ++ tTrigg
--              actionsToPerform = ((concat . map rspecReactions . stExitReactions) srcState
--                                  ++
--                                  tReacts
--                                  ++
--                                  (concat . map rspecReactions . stEntryReactions) dstState)
--          in
--            if (tSrc,tTrigg) `M.member` m
--            then Left (SmAmbiguityError srcState tTrigg)
--            else
--              let m' = M.insert (tSrc,tTrigg) actionsToPerform m
--              in addTransitions m' sMap ts
--
--
--addInternalReactions :: (M.Map (String,Event) TFuncSpec) -> [State] -> Either StateMachineError (M.Map (String,Event) TFuncSpec)
--addInternalReactions m ss = Right m
