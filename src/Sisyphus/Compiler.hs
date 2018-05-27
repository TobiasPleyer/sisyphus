module Sisyphus.Compiler where


import Data.Either (isRight)
import qualified Data.Map as M
import Sisyphus.Types


validateRSM :: RawStateMachine -> Either String ()
validateRSM = undefined

isValidRSM :: RawStateMachine -> Bool
isValidRSM = isRight . validateRSM


-- | This function is the key point of the implementation.
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
-- entry or exit reactions.
--
-- The transition functions will be assigned a unique name of the form
-- /from_{state name}_on_{event name}/. If this name appears more than once
-- this is treated as an ambiguity exception.
mkTransitionFunctions :: RawStateMachine -> M.Map (State,Event) TFuncSpec
mkTransitionFunctions rsm = addTransitions M.empty states transs
  where states = rsmStates rsm
        transs = rsmTransitions rsm
        addTransitions :: (M.Map (State,Event) TFuncSpec) -> [State] -> [TransitionSpec] -> (M.Map (State,Event) TFuncSpec)
        addTransitions = undefined
