{-# LANGUAGE RecordWildCards #-}

module Sisyphus.Compile where


import Control.Monad (forM_)
import Data.List ((\\))
import qualified Control.Monad.Trans.State.Lazy as TSL
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Sisyphus.Types
import Sisyphus.Util (addIngoingTransition, addOutgoingTransition)


runChecks :: StateMachine -> GrammarSummary
runChecks sm@SM{..} = TSL.execState (dedupe >> checkTransitions >> checkRest) initialSummary
  where
    initialSummary = GS sm eventSet actionSet stateSet stateSet [] []
    eventSet = S.fromList smEvents
    actionSet = S.fromList smActions
    stateSet = S.fromList $ M.keys smStates
    checkTransitions = forM_ smTransitions checkTransition


dedupe :: TSL.State GrammarSummary ()
dedupe = do
  dedupeEvents
  dedupeActions
  dedupeStates


dedupeEvents = getEvents >>= dedupeList "Event"
dedupeActions = getActions >>= dedupeList "Action"
dedupeStates = getStateNames >>= dedupeList "State"


dedupeList :: String -> [String] -> TSL.State GrammarSummary ()
dedupeList name items = do
  let uniques = S.toList $ S.fromList items
      redefs = items \\ uniques
  forM_ redefs $ \redef -> addWarning (name ++ " '" ++ redef ++ "' has already been defined")


checkRest :: TSL.State GrammarSummary ()
checkRest = do
  checkUnusedEvents
  checkUnusedActions
  checkUnreachableStates
  checkUnleavableStates


checkUnusedEvents = getUnusedEvents >>= report "Event" "unused"
checkUnusedActions = getUnusedActions >>= report "Action" "unused"
checkUnreachableStates = getUnreachableStates >>= report "State" "unreachable by any transition"
checkUnleavableStates = getUnleavableStates >>= report "State" "unleavable, but not a final state"


report name issue items = forM_ items $ \item -> addWarning (name ++ " '" ++ item ++ "' is " ++ issue)


checkTransition :: TransitionSpec -> TSL.State GrammarSummary ()
checkTransition t@(TSpec src dst trig guards reactions) = do
  let
    transEvents = transitionEvents t
    transActions = transitionActions t
  unuEvents <- getUnusedEvents
  unuActions <- getUnusedActions
  unrStates <- getUnreachableStates
  unlStates <- getUnleavableStates
  let
    unuEvents' = foldr S.delete unuEvents transEvents
    unuActions' = foldr S.delete unuActions transActions
    unrStates' = S.delete dst unrStates
    unlStates' = S.delete src unlStates
  setUnusedEvents unuEvents'
  setUnusedActions unuActions'
  setUnreachableStates unrStates'
  setUnleavableStates unlStates'
  events <- getEvents
  let unknownEvents = filter (flip notElem events) transEvents
  forM_ unknownEvents (\e -> addEvent e
                          >> addWarning ("Event '" ++ e ++ "' is undefined - adding default."))
  actions <- getActions
  let unknownActions = filter (flip notElem actions) transActions
  forM_ unknownActions (\a -> addAction a
                           >> addWarning ("Action '" ++ a ++ "' is undefined - adding default."))
  maybeSrcState <- getState src
  case maybeSrcState of
    Nothing -> do
      addWarning ("State '" ++ src ++ "' is undefined - adding default.")
      addState src (State src [] [] [] [] [t])
    Just (srcState) -> do
      let outgoings = stOutgoingTransitions srcState
      addState src srcState{stOutgoingTransitions=t:outgoings}
  maybeDstState <- getState dst
  case maybeDstState of
    Nothing -> do
      addWarning ("State '" ++ dst ++ "' is undefined - adding default.")
      addState dst (State dst [] [] [] [t] [])
    Just (dstState) -> do
      let ingoings = stIngoingTransitions dstState
      addState dst dstState{stIngoingTransitions=t:ingoings}


transitionEvents :: TransitionSpec -> [Event]
transitionEvents t@(TSpec src dst trigger guards reactions) =
  case trigger of
    Just e -> e:es
    Nothing -> es
  where
    es = map getEvent $ filter isEventEmit reactions
    getEvent (EventEmit e) = e


transitionActions :: TransitionSpec -> [Action]
transitionActions t@(TSpec src dst trigger guards reactions) = as
  where
    as = map getAction $ filter isActionCall reactions
    getAction (ActionCall a) = a

-- The summary state monad and the required utility functions

getStateMachine :: TSL.State GrammarSummary StateMachine
getStateMachine = TSL.state (\gs -> (stateMachine gs, gs))

setStateMachine :: StateMachine -> TSL.State GrammarSummary ()
setStateMachine sm = TSL.state (\gs -> ((), gs{stateMachine=sm}))


getName :: TSL.State GrammarSummary String
getName = do
  sm <- getStateMachine
  return $ smName sm


getEvents :: TSL.State GrammarSummary [Event]
getEvents = smEvents <$> getStateMachine

setEvents :: [Event] -> TSL.State GrammarSummary ()
setEvents es = do
  sm <- getStateMachine
  setStateMachine $ sm{smEvents=es}

addEvent :: Event -> TSL.State GrammarSummary ()
addEvent e = do
  es <- getEvents
  setEvents (e:es)


getActions :: TSL.State GrammarSummary [Action]
getActions = smActions <$> getStateMachine

setActions :: [Action] -> TSL.State GrammarSummary ()
setActions as = do
  sm <- getStateMachine
  setStateMachine $ sm{smActions=as}

addAction :: Action -> TSL.State GrammarSummary ()
addAction a = do
  as <- getActions
  setActions (a:as)


getStateMap :: TSL.State GrammarSummary (M.Map String State)
getStateMap = smStates <$> getStateMachine

getStateNames :: TSL.State GrammarSummary [String]
getStateNames = (M.keys . smStates) <$> getStateMachine

getState :: String -> TSL.State GrammarSummary (Maybe State)
getState s = (M.lookup s) <$> getStateMap

setStateMap :: (M.Map String State) -> TSL.State GrammarSummary ()
setStateMap ss = do
  sm <- getStateMachine
  setStateMachine $ sm{smStates=ss}

addState :: String -> State -> TSL.State GrammarSummary ()
addState n s = do
  sm <- getStateMap
  setStateMap (M.insert n s sm)


getTransitions :: TSL.State GrammarSummary [TransitionSpec]
getTransitions = smTransitions <$> getStateMachine

setTransitions :: [TransitionSpec] -> TSL.State GrammarSummary ()
setTransitions ts = do
  sm <- getStateMachine
  setStateMachine $ sm{smTransitions=ts}


getUnusedEvents :: TSL.State GrammarSummary (S.Set Event)
getUnusedEvents = TSL.state (\gs -> (unusedEvents gs, gs))

setUnusedEvents :: (S.Set Event) -> TSL.State GrammarSummary ()
setUnusedEvents ues = TSL.state (\gs -> ((), gs{unusedEvents=ues}))


getUnusedActions :: TSL.State GrammarSummary (S.Set Action)
getUnusedActions = TSL.state (\gs -> (unusedActions gs, gs))

setUnusedActions :: (S.Set Action) -> TSL.State GrammarSummary ()
setUnusedActions uas = TSL.state (\gs -> ((), gs{unusedActions=uas}))


getUnreachableStates :: TSL.State GrammarSummary (S.Set String)
getUnreachableStates = TSL.state (\gs -> (unreachableStates gs, gs))

setUnreachableStates :: (S.Set String) -> TSL.State GrammarSummary ()
setUnreachableStates urs = TSL.state (\gs -> ((), gs{unreachableStates=urs}))


getUnleavableStates :: TSL.State GrammarSummary (S.Set String)
getUnleavableStates = TSL.state (\gs -> (unleavableStates gs, gs))

setUnleavableStates :: (S.Set String) -> TSL.State GrammarSummary ()
setUnleavableStates uls = TSL.state (\gs -> ((), gs{unleavableStates=uls}))


getWarnings :: TSL.State GrammarSummary [String]
getWarnings = TSL.state (\gs -> (warnings gs, gs))

setWarnings :: [String] -> TSL.State GrammarSummary ()
setWarnings ws = TSL.state (\gs -> ((), gs{warnings=ws}))

addWarning :: String -> TSL.State GrammarSummary ()
addWarning w = TSL.state (\gs -> ((), gs{warnings=w:(warnings gs)}))


getErrors :: TSL.State GrammarSummary [String]
getErrors = TSL.state (\gs -> (errors gs, gs))

setErrors :: [String] -> TSL.State GrammarSummary ()
setErrors es = TSL.state (\gs -> ((), gs{errors=es}))

addError :: String -> TSL.State GrammarSummary ()
addError e = TSL.state (\gs -> ((), gs{errors=e:(errors gs)}))
