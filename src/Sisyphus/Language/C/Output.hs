module Sisyphus.Language.C.Output where


import Control.Exception (bracket)
import System.IO (IOMode(..), openFile, hClose, hPutStr, hPutStrLn)
import Sisyphus.Types
import Sisyphus.Util
import Sisyphus.Language.C.Util


outputSimple :: RawStateMachine -> IO ()
outputSimple rsm = do
  let fsm_name = maybe "FSM" id (rsmName rsm)
      header_file = fsm_name ++ ".h"
      source_file = fsm_name ++ ".c"
      header_content = interleave_shows nl2 [fsm_events
                                            ,fsm_states
                                            ,fsm_action_protos]
      fsm_events = outputEventEnum fsm_name (rsmEvents rsm)
      fsm_states = outputStateEnum fsm_name (rsmStates rsm)
      fsm_action_protos = outputActionProtos (rsmActions rsm)
  bracket
    (openFile header_file WriteMode)
    (hClose)
    $ \fh -> hPutStr fh (header_content "")
