{-# LANGUAGE RecordWildCards #-}

module Sisyphus.Compile where


import Control.Monad (forM_, mapM_)
import Data.Foldable (traverse_)
import qualified Control.Monad.Trans.State.Lazy as TSL
import qualified Data.Map.Strict as M
import qualified Data.Set as S

import Sisyphus.Types
import Sisyphus.Util (dedupeList, addIngoingTransition, addOutgoingTransition)


runChecks :: GrammarSummary -> GrammarSummary
runChecks gs = flip TSL.execState gs $ do
    checkDuplicates
    checkStates (smStates (stateMachine gs))
    checkTransitions (smTransitions (stateMachine gs))
    checkFinal


type SummaryM = TSL.State GrammarSummary


checkDuplicates :: SummaryM ()
checkDuplicates = do
  dedupeEvents
  dedupeActions


dedupeEvents = do
  events <- getEvents
  let (redefs,uniques) = dedupeList events
  forM_ redefs $ \redef -> addWarning ("Event '" ++ redef ++ "' has already been defined")
  setEvents uniques


dedupeActions = do
  actions <- getActions
  let (redefs,uniques) = dedupeList actions
  forM_ redefs $ \redef -> addWarning ("Action '" ++ redef ++ "' has already been defined")
  setActions uniques


checkFinal :: SummaryM ()
checkFinal = do
  checkUnusedEvents
  checkUnusedActions
  checkUnreachableStates
  checkUnleavableStates


checkUnusedEvents = getUnusedEvents >>= reportMany "Event" "unused"
checkUnusedActions = getUnusedActions >>= reportMany "Action" "unused"
checkUnreachableStates = getUnreachableStates >>= reportMany "State" "unreachable by any transition"
checkUnleavableStates = getUnleavableStates >>= reportMany "State" "unleavable, but not a final state"


report name issue item = addWarning (name ++ " '" ++ item ++ "' is " ++ issue)
reportMany name issue = mapM_ $ report name issue


checkStates = mapM_ checkState


checkState :: State -> SummaryM ()
checkState s@(State name attrs entries exits internals ingoings outgoings) = do
  let exitEvents = (concatMap reactionEvents exits)
      entryEvents = (concatMap reactionEvents entries)
      internalEvents = (concatMap reactionEvents internals)
      exitActions = (concatMap reactionActions exits)
      entryActions = (concatMap reactionActions entries)
      internalActions = (concatMap reactionActions internals)
  traverse_ markEventsUsed [entryEvents, exitEvents, internalEvents]
  traverse_ markActionsUsed [entryActions, exitActions, internalActions]
  events <- getEvents
  let unknownEvents = filter (flip notElem events) (concat [exitEvents, entryEvents, internalEvents])
  reportMany "Event" "undefined - adding default" unknownEvents
  forM_ unknownEvents addEvent
  actions <- getActions
  let unknownActions = filter (flip notElem actions) (concat [exitActions, entryActions, internalActions])
  reportMany "Action" "undefined - adding default" unknownActions
  forM_ unknownActions addAction


checkTransitions = mapM_ checkTransition


checkTransition :: TransitionSpec -> SummaryM ()
checkTransition t@(TSpec src dst trig guards reactions) = do
  let
    transEvents = transitionEvents t
    transActions = transitionActions t
  markEventsUsed transEvents
  markActionsUsed transActions
  markStateLeavable src
  markStateReachable dst
  events <- getEvents
  let unknownEvents = filter (flip notElem events) transEvents
  reportMany "Event" "undefined - adding default" unknownEvents
  forM_ unknownEvents addEvent
  actions <- getActions
  let unknownActions = filter (flip notElem actions) transActions
  reportMany "Action" "undefined - adding default" unknownActions
  forM_ unknownActions addAction
  maybeSrcState <- getState src
  case maybeSrcState of
    Nothing -> do
      report "State" "undefined - adding default" src
      addState src (State src [] [] [] [] [] [t])
    Just (srcState) -> do
      let outgoings = stOutgoingTransitions srcState
      addState src srcState{stOutgoingTransitions=t:outgoings}
  maybeDstState <- getState dst
  case maybeDstState of
    Nothing -> do
      report "State" "undefined - adding default" dst
      addState dst (State dst [] [] [] [] [t] [])
    Just (dstState) -> do
      let ingoings = stIngoingTransitions dstState
      addState dst dstState{stIngoingTransitions=t:ingoings}


allEvents :: [Reaction] -> [Event]
allEvents reactions = map getEvent $ filter isEventEmit reactions
  where
    getEvent (EventEmit e) = e


allActions :: [Reaction] -> [Action]
allActions reactions = map getAction $ filter isActionCall reactions
  where
    getAction (ActionCall a) = a


reactionEvents :: ReactionSpec -> [Event]
reactionEvents r@(RSpec trigger guards reactions) =
  case trigger of
    Just e -> e:es
    Nothing -> es
  where
    es = allEvents reactions


reactionActions :: ReactionSpec -> [Action]
reactionActions r@(RSpec trigger guards reactions) = allActions reactions


transitionEvents :: TransitionSpec -> [Event]
transitionEvents t@(TSpec src dst trigger guards reactions) =
  case trigger of
    Just e -> e:es
    Nothing -> es
  where
    es = allEvents reactions


transitionActions :: TransitionSpec -> [Action]
transitionActions t@(TSpec src dst trigger guards reactions) = allActions reactions

-- The summary state monad and the required utility functions

getStateMachine :: SummaryM StateMachine
getStateMachine = TSL.state (\gs -> (stateMachine gs, gs))

setStateMachine :: StateMachine -> SummaryM ()
setStateMachine sm = TSL.state (\gs -> ((), gs{stateMachine=sm}))


getName :: SummaryM String
getName = do
  sm <- getStateMachine
  return $ smName sm


getEvents :: SummaryM [Event]
getEvents = smEvents <$> getStateMachine

setEvents :: [Event] -> SummaryM ()
setEvents es = do
  sm <- getStateMachine
  setStateMachine $ sm{smEvents=es}

addEvent :: Event -> SummaryM ()
addEvent e = do
  es <- getEvents
  setEvents (e:es)


getActions :: SummaryM [Action]
getActions = smActions <$> getStateMachine

setActions :: [Action] -> SummaryM ()
setActions as = do
  sm <- getStateMachine
  setStateMachine $ sm{smActions=as}

addAction :: Action -> SummaryM ()
addAction a = do
  as <- getActions
  setActions (a:as)


getStateMap :: SummaryM (M.Map String State)
getStateMap = smStates <$> getStateMachine

getStateNames :: SummaryM [String]
getStateNames = (M.keys . smStates) <$> getStateMachine

getState :: String -> SummaryM (Maybe State)
getState s = (M.lookup s) <$> getStateMap

setStateMap :: (M.Map String State) -> SummaryM ()
setStateMap ss = do
  sm <- getStateMachine
  setStateMachine $ sm{smStates=ss}

addState :: String -> State -> SummaryM ()
addState n s = do
  sm <- getStateMap
  setStateMap (M.insert n s sm)

addDefaultState :: String -> SummaryM ()
addDefaultState n = addState n (State n [] [] [] [] [] [])


getTransitions :: SummaryM [TransitionSpec]
getTransitions = smTransitions <$> getStateMachine

setTransitions :: [TransitionSpec] -> SummaryM ()
setTransitions ts = do
  sm <- getStateMachine
  setStateMachine $ sm{smTransitions=ts}


getUnusedEvents :: SummaryM (S.Set Event)
getUnusedEvents = TSL.state (\gs -> (unusedEvents gs, gs))

setUnusedEvents :: (S.Set Event) -> SummaryM ()
setUnusedEvents ues = TSL.state (\gs -> ((), gs{unusedEvents=ues}))


getUnusedActions :: SummaryM (S.Set Action)
getUnusedActions = TSL.state (\gs -> (unusedActions gs, gs))

setUnusedActions :: (S.Set Action) -> SummaryM ()
setUnusedActions uas = TSL.state (\gs -> ((), gs{unusedActions=uas}))


getUnreachableStates :: SummaryM (S.Set String)
getUnreachableStates = TSL.state (\gs -> (unreachableStates gs, gs))

setUnreachableStates :: (S.Set String) -> SummaryM ()
setUnreachableStates urs = TSL.state (\gs -> ((), gs{unreachableStates=urs}))


getUnleavableStates :: SummaryM (S.Set String)
getUnleavableStates = TSL.state (\gs -> (unleavableStates gs, gs))

setUnleavableStates :: (S.Set String) -> SummaryM ()
setUnleavableStates uls = TSL.state (\gs -> ((), gs{unleavableStates=uls}))


getWarnings :: SummaryM [String]
getWarnings = TSL.state (\gs -> (warnings gs, gs))

setWarnings :: [String] -> SummaryM ()
setWarnings ws = TSL.state (\gs -> ((), gs{warnings=ws}))

addWarning :: String -> SummaryM ()
addWarning w = TSL.state (\gs -> ((), gs{warnings=w:(warnings gs)}))


getErrors :: SummaryM [String]
getErrors = TSL.state (\gs -> (errors gs, gs))

setErrors :: [String] -> SummaryM ()
setErrors es = TSL.state (\gs -> ((), gs{errors=es}))

addError :: String -> SummaryM ()
addError e = TSL.state (\gs -> ((), gs{errors=e:(errors gs)}))


markEventUsed :: Event -> SummaryM ()
markEventUsed e = do
  events <- getUnusedEvents
  let events' = S.delete e events
  setUnusedEvents events'


markEventsUsed :: [Event] -> SummaryM ()
markEventsUsed = mapM_ markEventUsed


markActionUsed :: Action -> SummaryM ()
markActionUsed a = do
  actions <- getUnusedActions
  let actions' = S.delete a actions
  setUnusedActions actions'


markActionsUsed :: [Action] -> SummaryM ()
markActionsUsed = mapM_ markActionUsed


markStateReachable :: String -> SummaryM ()
markStateReachable s = do
  states <- getUnreachableStates
  let states' = S.delete s states
  setUnreachableStates states'


markStatesReachable :: [String] -> SummaryM ()
markStatesReachable = mapM_ markStateReachable


markStateLeavable :: String -> SummaryM ()
markStateLeavable s = do
  states <- getUnleavableStates
  let states' = S.delete s states
  setUnleavableStates states'


markStatesLeavable :: [String] -> SummaryM ()
markStatesLeavable = mapM_ markStateLeavable
