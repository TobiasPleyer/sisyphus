{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Sisyphus.Language.C
( renderCSimple
) where


import Conduit
import Control.Applicative (liftA2)
import Control.Monad (forM_)
import Data.Conduit
import Data.Monoid
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import System.FilePath
import Text.Ginger
import Sisyphus.Types
import Sisyphus.Ginger
import Sisyphus.Util
import Sisyphus.Language.Template


cTemplateHeaderSimple = "C/fsm.h.tmpl"
cTemplateSourceSimple = "C/fsm.c.tmpl"
cTemplateSourceSimplePre = "C/fsm.c_pre.tmpl"
cTemplateSourceSimplePost = "C/fsm.c_post.tmpl"


renderCSimple = renderCSimple_Hybrid


renderCSimple_GingerOnly :: StateMachine -> FilePath -> IO ()
renderCSimple_GingerOnly sm outDir = do
  let
    name = smName sm
    context  = mkContext sm
  renderFromFile context outDir cTemplateHeaderSimple (name <.> "h")
  renderFromFile context outDir cTemplateSourceSimple (name <.> "c")


renderCSimple_Hybrid :: StateMachine -> FilePath -> IO ()
renderCSimple_Hybrid sm outDir = do
  let
    name = smName sm
    context  = mkContext sm

  renderFromFile context outDir cTemplateHeaderSimple (name <.> "h")

  cSourceTemplatePre <- parseGingerFile defaultTemplateLoader cTemplateSourceSimplePre
  cSourceTemplatePost <- parseGingerFile defaultTemplateLoader cTemplateSourceSimplePost

  let parsedTemplates = liftA2 (,) cSourceTemplatePre cSourceTemplatePost
  case parsedTemplates of
    Left err -> do
      printParseError err
    Right (preTmpl,postTmpl) -> do
      let
        source = do
          yield $ runGinger context preTmpl
          emitEntries sm
          emitExits sm
          emitTransitions sm
          emitTransitionTable sm
          yield $ runGinger context postTmpl
      runConduitRes $ source
                    .| encodeUtf8C
                    .| sinkFile (outDir </> name <.> "c")


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
