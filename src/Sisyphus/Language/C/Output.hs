module Sisyphus.Language.C.Output where


import Control.Exception (bracket)
import System.IO (IOMode(..), openFile, hClose, hPutStr, hPutStrLn)
import Sisyphus.Types
import Sisyphus.Util
import Sisyphus.Language.C.Util


header_generic_pre fsm_name =
  let define_guard = fsm_name ++ "_H"
  in str ("#ifndef " ++ define_guard)
   . nl
   . str ("#define " ++ define_guard)


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


header_generic_post fsm_name =
    str "struct " . str fsm_name . str "_t {" . nl
  . str "    " . str fsm_name . str "_state_t current_state;" . nl
  . char '}' . nl2
  . str "struct " . str fsm_name . str "_t " . str fsm_name . char ';' . nl2
  . str ("void " ++ fsm_name ++ "_Init(void);") . nl2
  . str ("unsigened char " ++ fsm_name ++ "_Dispatch(" ++ fsm_name ++ "_event_t event);") . nl2
  . str "#endif"


outputSimple :: RawStateMachine -> IO ()
outputSimple rsm = do
  let fsm_name = maybe "FSM" id (rsmName rsm)
      header_file = fsm_name ++ ".h"
      source_file = fsm_name ++ ".c"
      header_content = interleave_shows nl2 [header_generic_pre fsm_name
                                            ,outputEventEnum fsm_name (rsmEvents rsm)
                                            ,outputStateEnum fsm_name (rsmStates rsm)
                                            ,outputActionProtos (rsmActions rsm)
                                            ,header_generic_post fsm_name]
  bracket
    (openFile header_file WriteMode)
    (hClose)
    $ \fh -> hPutStr fh (header_content "")
