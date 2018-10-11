{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Sisyphus.Language.C
( renderCSimple
) where


import Conduit
import Control.Monad (forM_)
import Data.Conduit
import Data.Either (fromRight)
import Data.Monoid
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
              emitEntries sm
              emitExits sm
              emitTransitions sm
              yield post
          runConduitRes $ source
                        .| encodeUtf8C
                        .| sinkFile outSourceFile


emitEntries :: StateMachine -> ConduitT i T.Text (ResourceT IO) ()
emitEntries SM{..} = do
  yield "/*** ENTRY FUNCTIONS ***/\n"
  forM_ (M.elems smStates) (emitEntriesSt (T.pack smName))


emitExits :: StateMachine -> ConduitT i T.Text (ResourceT IO) ()
emitExits SM{..} = do
  yield "/*** EXIT FUNCTIONS ***/\n"
  forM_ (M.elems smStates) (emitExitsSt (T.pack smName))


emitEntriesSt :: T.Text -> State -> ConduitT i T.Text (ResourceT IO) ()
emitEntriesSt name State{..} = do
  if (not $ null $ stEntryReactions) then do
    yield $ "void " <> name <> "_" <> (T.pack stName) <> "__entry(" <> name <> "_t* pSM)\n{\n"
    forM_ stEntryReactions $ \e -> forM_ (rspecReactions e) $ \r -> do
      case r of
        ActionCall a -> yield $ "\t" <> (T.pack a) <> "();\n"
        EventEmit ev -> yield $ "\t" <> name <> "_AddSignal(pSM, " <> name <> "_" <> (T.pack ev) <> ");\n"
    yield "}\n\n"
  else return ()


emitExitsSt :: T.Text -> State -> ConduitT i T.Text (ResourceT IO) ()
emitExitsSt name State{..} = do
  if (not $ null $ stExitReactions) then do
    yield $ "void " <> name <> "_" <> (T.pack stName) <> "__exit(" <> name <> "_t* pSM)\n{\n"
    forM_ stExitReactions $ \e -> forM_ (rspecReactions e) $ \r -> do
      case r of
        ActionCall a -> yield $ "\t" <> (T.pack a) <> "();\n"
        EventEmit ev -> yield $ "\t" <> name <> "_AddSignal(pSM, " <> name <> "_" <> (T.pack ev) <> ");\n"
    yield "}\n\n"
  else return ()


emitTransitions sm@SM{..} = do
  yield "/*** TRANSITION FUNCTIONS ***/\n"
  forM_ (M.elems smStates) (emitTransitionsSt sm (T.pack smName))


emitTransitionsSt sm name s = do
  forM_ (stOutgoingTransitions s) $ \o -> do
    let
      trigger = maybe "" T.pack (tspecTrigger o)
      dstState = (smStates sm) M.! (tspecDst o)
    yield $ "void " <> name <> "_" <> (T.pack (stName s)) <> "__on_" <> trigger <> "(" <> name <> "_t* pSM)\n{\n"
    if (not $ null $ (stExitReactions s)) then
      yield $ "\t" <> name <> "_" <> (T.pack (stName s)) <> "__exit(" <> name <> "_t* pSM);\n"
    else return ()
    if (not $ null $ (stEntryReactions dstState)) then
      yield $ "\t" <> name <> "_" <> (T.pack (stName dstState)) <> "__entry(" <> name <> "_t* pSM);\n"
    else return ()
    yield "}\n\n"
