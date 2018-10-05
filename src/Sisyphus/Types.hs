{-# LANGUAGE RecordWildCards #-}

module Sisyphus.Types where


import qualified Data.Map.Strict as M
import qualified Data.IntMap.Strict as IM
import qualified Data.Set as S


data StateMachineError = SmUnknownEventError String
                       | SmUnknownActionError String
                       | SmUnknownStateError String
                       | SmAmbiguityError State Event
                       | SmNoTriggerError (Either State TransitionSpec)
                       | SmCustomError String

instance Show StateMachineError where
  show (SmUnknownEventError s) = "Event " ++ s ++ " is not defined in the grammar file!"
  show (SmUnknownActionError s) = "Action " ++ s ++ " is not defined in the grammar file!"
  show (SmUnknownStateError s) = "State " ++ s ++ " is not defined in the grammar file!"
  show (SmAmbiguityError s e) = "Ambiguous transition rules for state " ++ (stName s) ++ " for event " ++ e ++ "!"
  show (SmNoTriggerError (Left s)) = "State " ++ (stName s) ++ " has an internal reaction without trigger!"
  show (SmNoTriggerError (Right t)) = "Transition from state " ++ (tspecSrc t) ++ " to state " ++ (tspecDst t) ++ " without trigger!"
  show (SmCustomError s) = s

  showsPrec _ _ = id

data StateMachine = SM
  { smName        :: String
  , smEvents      :: [Event]
  , smActions     :: [Action]
  , smStates      :: M.Map String State
  , smTransitions :: [TransitionSpec]
  }

instance Show StateMachine where
  show SM{..} = (init . unlines) $ strName : (concat [strEvents, strActions, strStates])
    where
      strName = "Name: " ++ smName
      strEvents = "Events" : (map ("  " ++) smEvents)
      strActions = "Actions" : (map ("  " ++) smActions)
      strStates = "States" : (map ((init . unlines) . map ("  " ++) . lines . show) (M.elems smStates))
  showsPrec _ r s = (show r) ++ s

data GrammarSummary = GS
  { stateMachine :: StateMachine
  , unusedEvents :: S.Set Event
  , unusedActions :: S.Set Action
  , unreachableStates :: S.Set String
  , unleavableStates :: S.Set String
  , warnings :: [String]
  , errors :: [String]
  } deriving (Show)

type Event  = String
type Action = String
type Guard  = String
type TFuncSpec = [Reaction]

data State = State
  { stName                :: String
  , stEntryReactions      :: [ReactionSpec]
  , stExitReactions       :: [ReactionSpec]
  , stInternalReactions   :: [ReactionSpec]
  , stIngoingTransitions  :: [TransitionSpec]
  , stOutgoingTransitions :: [TransitionSpec]
  }

instance Show State where
  show State{..} = (init . unlines) $ stName : (concat [strEntries, strExits, strInternals, strIngoings, strOutgoings])
    where
      strEntries = map (("  entry: " ++) . show) stEntryReactions
      strExits = map (("  exit: " ++) . show) stExitReactions
      strInternals = map (("  internal: " ++) . show) stInternalReactions
      strIngoings = map (("  in: " ++) . show) stIngoingTransitions
      strOutgoings = map (("  out: " ++) . show) stOutgoingTransitions
  showsPrec _ r s = (show r) ++ s

data ReactionSpec = RSpec
  { rspecTrigger   :: Maybe Event
  , rspecGuards    :: [Guard]
  , rspecReactions :: [Reaction]
  } deriving (Show)

data TransitionSpec = TSpec
  { tspecSrc       :: String
  , tspecDst       :: String
  , tspecTrigger   :: Maybe Event
  , tspecGuards    :: [Guard]
  , tspecReactions :: [Reaction]
  } deriving (Show)

data Reaction = ActionCall Action
              | EventEmit Event

isActionCall (ActionCall _) = True
isActionCall _              = False

isEventEmit (EventEmit _) = True
isEventEmit _             = False

instance Show Reaction where
  show (ActionCall a) = a ++ "()"
  show (EventEmit e) = '^' : e
  showsPrec _ r s = (show r) ++ s

data ReactionClassifier = ReactEntry
                        | ReactExit
                        | ReactInternal String
                        deriving (Show)

isEntry ReactEntry = True
isEntry _          = False

isExit ReactExit = True
isExit _         = False

isInternal (ReactInternal _) = True
isInternal _                 = False
