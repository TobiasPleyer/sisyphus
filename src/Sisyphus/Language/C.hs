{-# LANGUAGE OverloadedStrings #-}

module Sisyphus.Language.C
( renderCSimple, mkCContext
) where


import qualified Data.Map.Strict as M
import Data.Maybe (fromJust, isJust)
import qualified Data.Text as T
import System.Exit (ExitCode(..), exitWith)
import System.FilePath
import Text.Ginger
       (makeContextText, Template, toGVal, runGinger, parseGingerFile, VarName)
import Text.Ginger.GVal
import Sisyphus.Types
import Sisyphus.Ginger
import Sisyphus.Language.Template (defaultTemplateLoader, renderTemplate)


cTemplateHeaderSimple = "C/fsm.h.tmpl"
cTemplateSourceSimple = "C/fsm.c.tmpl"


renderCSimple :: StateMachine -> FilePath -> IO ()
renderCSimple sm outDir = do
  let outHeaderFile = outDir </> (smName sm) <.> "h"
  let outSourceFile = outDir </> (smName sm) <.> "c"
  cHeaderTempl <- parseGingerFile defaultTemplateLoader cTemplateHeaderSimple
  case cHeaderTempl of
    Left err -> do
      putStrLn "Failed to load the header target!"
    Right cHeaderTemplate -> do
      let cContext  = mkCContext sm
      renderTemplate cContext cHeaderTemplate outHeaderFile
  cSourceTempl <- parseGingerFile defaultTemplateLoader cTemplateSourceSimple
  case cSourceTempl of
    Left err -> do
      putStrLn "Failed to load the source target!"
    Right cSourceTemplate -> do
      let cContext  = mkCContext sm
      renderTemplate cContext cSourceTemplate outSourceFile


mkCContext sm = makeContextText contextLookup
  where
    contextLookup key = (M.!) contextMap key
    contextMap = M.fromList [("FSM_NAME", toGVal fsm_name)
                            ,("FSM_EVENTS", toGVal fsm_events)
                            ,("FSM_STATES", toGVal fsm_states)
                            ,("FSM_ACTIONS", toGVal fsm_actions)
                            ,("FSM_START_STATE", toGVal (T.pack "Closed"))
                            ,("FSM_STATE_MACHINE", toGVal sm)
                            ,("triggers", fromFunction triggers)
                            ]
    fsm_name = T.pack $ smName sm
    fsm_events = map T.pack $ smEvents sm
    fsm_states = map T.pack $ M.keys (smStates sm)
    fsm_actions = map T.pack $ smActions sm
    triggers args =
      let
        event = T.unpack (asText (snd (head args)))
        state = T.unpack (asText (snd (head (tail args))))
      in return
         . toGVal
         . not
         . null
         . filter (==event)
         . map fromJust
         . filter isJust
         . map tspecTrigger
         $ stOutgoingTransitions ((smStates sm) M.! state)
