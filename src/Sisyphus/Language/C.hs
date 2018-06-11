{-# LANGUAGE OverloadedStrings #-}

module Sisyphus.Language.C
( renderCSimple, mkCContext
) where


import qualified Data.Map.Strict as M
import qualified Data.Text as T
import System.Exit (ExitCode(..), exitWith)
import Text.Ginger
       (makeContextText, Template, toGVal, runGinger, parseGingerFile, VarName)
import Text.Ginger.GVal (ToGVal, GVal)
import Sisyphus.Types
import Sisyphus.Language.Template (defaultTemplateLoader, renderTemplate)


cTemplateHeaderSimple = "C/fsm.h.tmpl"
cTemplateSourceSimple = "C/fsm.c.tmpl"


renderCSimple :: ValidatedStateMachine -> FilePath -> IO ()
renderCSimple vsm outFile = do
  cHeaderTempl <- parseGingerFile defaultTemplateLoader cTemplateHeaderSimple
  case cHeaderTempl of
    Left err -> do
      putStrLn "Failed to load the target!"
      exitWith (ExitFailure 2)
    Right cHeaderTemplate -> do
      let
        cContext  = mkCContext vsm
      renderTemplate cContext cHeaderTemplate outFile


mkCContext vsm = makeContextText contextLookup
  where
    contextLookup key = (M.!) contextMap key
    contextMap = M.fromList [("FSM_NAME", toGVal fsm_name)
                            ,("FSM_EVENTS", toGVal fsm_events)
                            ,("FSM_STATES", toGVal fsm_states)
                            ,("FSM_ACTIONS", toGVal fsm_actions)]
    fsm_name = T.pack $ vsmName vsm
    fsm_events = map T.pack $ vsmEvents vsm
    fsm_states = map (T.pack . stName) $ vsmStates vsm
    fsm_actions = map T.pack $ vsmActions vsm
