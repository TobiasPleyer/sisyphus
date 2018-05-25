module Sisyphus.Language.C.Util where


import Sisyphus.Types
import Sisyphus.Util


outputSimpleEnum :: Maybe String  -- ^ optional name put at the end of the enum
                 -> String        -- ^ the name of the enum type
                 -> [String]      -- ^ the enum elements
                 -> ShowS         -- ^ the final output function
outputSimpleEnum counter_name enum_name ss =
    str "typedef enum {"
  . str "\n    "
  . (interleave_shows
        (str ",\n    ")
        (map str ss))
  . (maybe id (\cname -> str "\n    "
                       . str cname) counter_name)
  . str ",\n} "
  . str enum_name
  . char ';'


outputStateEnum :: String -> [State] -> ShowS
outputStateEnum fsm_name = outputSimpleEnum (Just (fsm_name ++ "_NUM_STATES")) (fsm_name ++ "_state_t") . map stName


outputEventEnum :: String -> [Event] -> ShowS
outputEventEnum fsm_name = outputSimpleEnum (Just (fsm_name ++ "_NUM_EVENTS")) (fsm_name ++ "_event_t")


outputActionProtos :: [Action] -> ShowS
outputActionProtos as =
    str "typedef void action_function_t (void);" . nl2
  . interleave_shows nl as'
  where
    as' = map (enclose "action_function_t " ";") as


outputHandlerProtos :: RawStateMachine -> ShowS
outputHandlerProtos rsm =
  let states = rsmStates rsm
      transitions = rsmTransitions rsm
  in  str "typedef void handler_function_t (void);" . nl2


outputTransitionTable = undefined
