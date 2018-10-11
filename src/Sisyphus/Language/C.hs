{-# LANGUAGE OverloadedStrings #-}

module Sisyphus.Language.C
( renderCSimple
) where


import Conduit
import Data.Conduit
import Data.Either (fromRight)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import System.Exit (ExitCode(..), exitWith)
import System.FilePath
import Text.Ginger
       (makeContextText, Template, toGVal, runGinger, parseGingerFile, VarName)
import Text.Ginger.GVal (ToGVal, GVal)
import Sisyphus.Types
import Sisyphus.Language.Template (defaultTemplateLoader, renderTemplate)


cTemplateHeaderSimple = "C/fsm.h.tmpl"
cTemplateSourceSimple = "C/fsm.c.tmpl"
cTemplateSourceSimplePre = "C/fsm.c_pre.tmpl"
cTemplateSourceSimplePost = "C/fsm.c_post.tmpl"


renderCSimple = renderCSimple_Hybrid


renderCSimple_GingerOnly :: StateMachine -> FilePath -> IO ()
renderCSimple_GingerOnly sm outDir = do
  renderCHeaderSimple_GingerOnly sm outDir
  renderCSourceSimple_GingerOnly sm outDir


renderCHeaderSimple_GingerOnly :: StateMachine -> FilePath -> IO ()
renderCHeaderSimple_GingerOnly sm outDir = do
  let outHeaderFile = outDir </> (smName sm) <.> "h"
  cHeaderTempl <- parseGingerFile defaultTemplateLoader cTemplateHeaderSimple
  case cHeaderTempl of
    Left err -> do
      putStrLn "Failed to load the header template!"
    Right cHeaderTemplate -> do
      let cContext  = mkCContext sm
      renderTemplate cContext cHeaderTemplate outHeaderFile


renderCSourceSimple_GingerOnly :: StateMachine -> FilePath -> IO ()
renderCSourceSimple_GingerOnly sm outDir = do
  let outSourceFile = outDir </> (smName sm) <.> "c"
  cSourceTempl <- parseGingerFile defaultTemplateLoader cTemplateSourceSimple
  case cSourceTempl of
    Left err -> do
      putStrLn "Failed to load the source template!"
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
                            ,("FSM_START_STATE", toGVal (T.pack "Closed"))]
    fsm_name = T.pack $ smName sm
    fsm_events = map T.pack $ smEvents sm
    fsm_states = map T.pack $ M.keys (smStates sm)
    fsm_actions = map T.pack $ smActions sm


renderCSimple_Hybrid :: StateMachine -> FilePath -> IO ()
renderCSimple_Hybrid sm outDir = do
  renderCHeaderSimple_GingerOnly sm outDir
  let
    outSourceFile = outDir </> (smName sm) <.> "c"
    cContext  = mkCContext sm
  cSourceTemplatePre <- parseGingerFile defaultTemplateLoader cTemplateSourceSimplePre
  case cSourceTemplatePre of
    Left err -> do
      putStrLn "Failed to load the source pre template!"
    Right cSourceTemplatePre -> do
      let pre = runGinger cContext cSourceTemplatePre
      cSourceTemplatePost <- parseGingerFile defaultTemplateLoader cTemplateSourceSimplePost
      case cSourceTemplatePost of
        Left err -> do
          putStrLn "Failed to load the source post template!"
        Right cSourceTemplatePost -> do
          let
            post = runGinger cContext cSourceTemplatePost
            source = do
              yield pre
              yield post
          runConduitRes $ source
                        .| encodeUtf8C
                        .| sinkFile outSourceFile
