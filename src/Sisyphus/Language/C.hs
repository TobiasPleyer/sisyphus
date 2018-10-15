{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Sisyphus.Language.C
( renderCSimple
) where


import Conduit
import Control.Applicative (liftA2)
import Control.Monad (forM_)
import Data.Conduit
import Data.Either (fromRight)
import Data.Monoid
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust, isJust)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import System.Exit (ExitCode(..), exitWith)
import System.FilePath
import Text.Ginger
import Text.Ginger.GVal
import Sisyphus.Types
import Sisyphus.Ginger
import Sisyphus.Util
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
        event = T.unpack (asText (snd (args !! 0)))
        state = T.unpack (asText (snd (args !! 1)))
      in return
         . toGVal
         . not
         . null
         . filter (==event)
         . map fromJust
         . filter isJust
         . map tspecTrigger
         $ stOutgoingTransitions ((smStates sm) M.! state)


renderCSimple_Hybrid :: StateMachine -> FilePath -> IO ()
renderCSimple_Hybrid sm outDir = do
  renderCHeaderSimple_GingerOnly sm outDir
  let
    outSourceFile = outDir </> (smName sm) <.> "c"
    cContext  = mkCContext sm
  cSourceTemplatePre <- parseGingerFile defaultTemplateLoader cTemplateSourceSimplePre
  cSourceTemplatePost <- parseGingerFile defaultTemplateLoader cTemplateSourceSimplePost
  let parsedTemplates = liftA2 (,) cSourceTemplatePre cSourceTemplatePost
  case parsedTemplates of
    Left err -> do
      printGingerParseError err
    Right (preTmpl,postTmpl) -> do
      let
        pre = runGinger cContext preTmpl
        post = runGinger cContext postTmpl
        source = do
          yield pre
          emitEntries sm
          emitExits sm
          emitTransitions sm
          emitTransitionTable sm
          yield post
      runConduitRes $ source
                    .| encodeUtf8C
                    .| sinkFile outSourceFile


printGingerParseError err = do
  maybe (return ()) print (peSourcePosition err)
  putStrLn (peErrorMessage err)


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
      yield $ "\t" <> name <> "_" <> (T.pack (stName s)) <> "__exit(pSM);\n"
    else return ()
    forM_ (tspecReactions o) $ \r -> do
      case r of
        ActionCall a -> yield $ "\t" <> (T.pack a) <> "();\n"
        EventEmit ev -> yield $ "\t" <> name <> "_AddSignal(pSM, " <> name <> "_" <> (T.pack ev) <> ");\n"
    if (not $ null $ (stEntryReactions dstState)) then
      yield $ "\t" <> name <> "_" <> (T.pack (stName dstState)) <> "__entry(pSM);\n"
    else return ()
    yield $ "\tpSM->current_state = " <> name <> "_" <> (T.pack (stName dstState)) <> ";\n"
    yield "}\n\n"
  forM_ (stInternalReactions s) $ \i -> do
    let
      trigger = maybe "" T.pack (rspecTrigger i)
    yield $ "void " <> name <> "_" <> (T.pack (stName s)) <> "__on_" <> trigger <> "(pSM)\n{\n"
    forM_ (rspecReactions i) $ \r -> do
      case r of
        ActionCall a -> yield $ "\t" <> (T.pack a) <> "();\n"
        EventEmit ev -> yield $ "\t" <> name <> "_AddSignal(pSM, " <> name <> "_" <> (T.pack ev) <> ");\n"
    yield "}\n\n"


emitTransitionTable sm = do
  let
    name = T.pack (smName sm)
  yield $ "transition_function_t* " <> name <> "_transition_table[" <> name <> "_NUM_STATES][" <> name <> "_NUM_EVENTS] = {\n"
  forM_ (M.elems (smStates sm)) (emitTransitionTableRow sm name)
  yield "};\n\n"


emitTransitionTableRow sm name s = do
  let
    events = smEvents sm
    first = head events
    rest = tail events
    state = T.pack (stName s)
  yield "\t{"
  -- The first element of the row does not emit a comma
  if (s `isTriggeredBy` first)
  then yield $ name <> "_" <> state <> "__on_" <> (T.pack first)
  else yield "NULL"
  forM_ rest $ \e ->
    if (s `isTriggeredBy` e)
    then yield $ ", " <> name <> "_" <> state <> "__on_" <> (T.pack e)
    else yield ", NULL"
  yield "},\n"
