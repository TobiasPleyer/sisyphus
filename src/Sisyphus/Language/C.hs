{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Sisyphus.Language.C
( renderCSimple
) where


import Conduit
import Control.Applicative (liftA2)
import Control.Monad (forM_)
import Data.Conduit
import Data.Maybe (fromJust)
import Data.Monoid
import qualified Data.Array as A
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import System.FilePath
import Text.Ginger
import Sisyphus.SisSyn
import Sisyphus.Util
import Sisyphus.Language.Template


renderCSimple :: StateMachine -> FilePath -> IO ()
renderCSimple sm outDir = do
  renderHeader sm outDir
  renderSource sm outDir

renderHeader :: StateMachine -> FilePath -> IO ()
renderHeader sm@SM{..} outDir = do
  let
    target = smName <.> "h"
    cHeaderTmpl = "C/fsm.h.tmpl"
    context = makeContextText (
      (M.!) (M.fromList [("FSM_NAME", toGVal smName)
                        ,("FSM_EVENTS", toGVal $ A.elems smEvents)
                        ,("FSM_ACTIONS", toGVal $ A.elems smActions)
                        ,("FSM_STATES", toGVal $ map stnName (A.elems smStates))
                        ]
            )
      )
  renderFromFile context outDir cHeaderTmpl target


renderSource :: StateMachine -> FilePath -> IO ()
renderSource sm@SM{..} outDir = do
  let
    target = outDir </> smName <.> "c"
    -- We need a better checking here
    startState = (A.!) smStates $ fromJust $ srInitial $ (A.!) smRegions 0
    context = makeContextText (
      (M.!) (M.fromList [("FSM_NAME", toGVal smName)
                        ,("FSM_EVENTS", toGVal $ A.elems smEvents)
                        ,("FSM_STATES", toGVal $ map stnName (A.elems smStates))
                        ,("FSM_START_STATE", toGVal $ stnName startState)
                        ,("HAS_START_STATE_ENTRY", toGVal $ not $ null $ getAllEntries $ stnBehaviors startState)
                        ]
            )
      )
  preTmpl <- parseGingerFile defaultTemplateLoader "C/fsm.c_pre.tmpl"
  postTmpl <- parseGingerFile defaultTemplateLoader "C/fsm.c_post.tmpl"
  case (liftA2 (,) preTmpl postTmpl) of
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
                    .| sinkFile target


emitEntries :: StateMachine -> ConduitT i T.Text (ResourceT IO) ()
emitEntries SM{..} = do
  yield "/*** ENTRY FUNCTIONS ***/\n"
  forM_ (A.elems smStates) (emitEntriesSt (T.pack smName))


emitExits :: StateMachine -> ConduitT i T.Text (ResourceT IO) ()
emitExits SM{..} = do
  yield "/*** EXIT FUNCTIONS ***/\n"
  forM_ (A.elems smStates) (emitExitsSt (T.pack smName))


emitEntriesSt :: T.Text -> (SisState Id) -> ConduitT i T.Text (ResourceT IO) ()
emitEntriesSt name state@STNormal{..} = do
  let entries = getAllEntries stnBehaviors
  if (not $ null $ entries) then do
    yield $ "void " <> name <> "_" <> (T.pack stnName) <> "__entry(" <> name <> "_t* pSM)\n{\n"
    forM_ entries $ \e ->  do
      case (seKind e) of
        SEKAction -> yield $ "\t" <> (T.pack $ seName e) <> "();\n"
        SEKEvent -> yield $ "\t" <> name <> "_AddSignal(pSM, " <> name <> "_" <> (T.pack $ seName e) <> ");\n"
    yield "}\n\n"
  else return ()


emitExitsSt :: T.Text -> (SisState Id) -> ConduitT i T.Text (ResourceT IO) ()
emitExitsSt name state@STNormal{..} = do
  let exits = getAllExits stnBehaviors
  if (not $ null $ exits) then do
    yield $ "void " <> name <> "_" <> (T.pack stnName) <> "__exit(" <> name <> "_t* pSM)\n{\n"
    forM_ exits $ \e -> do
      case (seKind e) of
        SEKAction -> yield $ "\t" <> (T.pack $ seName e) <> "();\n"
        SEKEvent -> yield $ "\t" <> name <> "_AddSignal(pSM, " <> name <> "_" <> (T.pack $ seName e) <> ");\n"
    yield "}\n\n"
  else return ()


emitTransitions sm@SM{..} = do
  yield "/*** TRANSITION FUNCTIONS ***/\n"
  forM_ (A.elems smStates) (emitTransitionsSt sm (T.pack smName))


emitTransitionsSt sm name s = do
  forM_ (stnOutgoingTransitions s) $ \o -> do
    let
      trigger = maybe "" T.pack (stTrigger o)
      dstState = (smStates sm) A.! (stDst o)
    yield $ "void " <> name <> "_" <> (T.pack (stnName s)) <> "__on_" <> trigger <> "(" <> name <> "_t* pSM)\n{\n"
    if (not $ null $ (getAllExits $ stnBehaviors s)) then
      yield $ "\t" <> name <> "_" <> (T.pack (stnName s)) <> "__exit(pSM);\n"
    else return ()
    forM_ (stEffects o) $ \eff -> do
      case (seKind eff) of
        SEKAction -> yield $ "\t" <> (T.pack $ seName eff) <> "();\n"
        SEKEvent -> yield $ "\t" <> name <> "_AddSignal(pSM, " <> name <> "_" <> (T.pack $ seName eff) <> ");\n"
    if (not $ null $ (getAllEntries $ stnBehaviors dstState)) then
      yield $ "\t" <> name <> "_" <> (T.pack (stnName dstState)) <> "__entry(pSM);\n"
    else return ()
    yield $ "\tpSM->current_state = " <> name <> "_" <> (T.pack (stnName dstState)) <> ";\n"
    yield "}\n\n"
  forM_ (stnInternalTransitions s) $ \i -> do
    let
      trigger = maybe "" T.pack (stTrigger i)
    yield $ "void " <> name <> "_" <> (T.pack (stnName s)) <> "__on_" <> trigger <> "(pSM)\n{\n"
    forM_ (stEffects i) $ \eff -> do
      case (seKind eff) of
        SEKAction -> yield $ "\t" <> (T.pack $ seName eff) <> "();\n"
        SEKEvent -> yield $ "\t" <> name <> "_AddSignal(pSM, " <> name <> "_" <> (T.pack $ seName eff) <> ");\n"
    yield "}\n\n"


emitTransitionTable sm = do
  let name = T.pack (smName sm)
  yield $ "transition_function_t* " <> name <> "_transition_table[" <> name <> "_NUM_STATES][" <> name <> "_NUM_EVENTS] = {\n"
  forM_ (A.elems (smStates sm)) (emitTransitionTableRow sm name)
  yield "};\n\n"


emitTransitionTableRow sm name s = do
  let
    events = A.elems $ smEvents sm
    first = head events
    rest = tail events
    state = T.pack (stnName s)
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