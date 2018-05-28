module Sisyphus.Types where


type StateMachineError = SmUnknownEventError String
                       | SmUnknownActionError String
                       | SmUnknownStateError String
                       | SmAmbiguityError State Event
                       | SmNoTriggerError (Either State TransitionSpec)
                       | SmCustomError String

instance Show StateMachineError where
  show SmUnknownEventError s = "Event " ++ s ++ " is not defined in the grammar file!"
  show SmUnknownActionError s = "Action " ++ s ++ " is not defined in the grammar file!"
  show SmUnknownStateError s = "State " ++ s ++ " is not defined in the grammar file!"
  show SmAmbiguityError s e = "Ambiguous transition rules for state " ++ (stName s) ++ " for event " ++ e ++ "!"
  show SmNoTriggerError (Left s) = "State " ++ (stName s) ++ " has an internal reaction without trigger!"
  show SmNoTriggerError (Right t) = "Transition from state " ++ (stName (tspecSrc t)) ++ " to state " ++ (stName (tspecDst t)) ++ " without trigger!"
  show SmCustomError s = s

  showsPrec _ _ = id

data RawStateMachine = RSM
  { rsmName        :: String
  , rsmEvents      :: [Event]
  , rsmActions     :: [Action]
  , rsmStates      :: [State]
  , rsmTransitions :: [TransitionSpec]
  } deriving (Show)

type Event  = String
type Action = String
type Guard  = String
type TFuncSpec = [Reaction]

data State = State
  { stName              :: String
  , stEntryReactions    :: [ReactionSpec]
  , stExitReactions     :: [ReactionSpec]
  , stInternalReactions :: [ReactionSpec]
  , stIngoing           :: [TransitionSpec]
  , stOutgoing          :: [TransitionSpec]
  } deriving (Show)

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

isActionCall ActionCall _ = True
isActionCall _            = False

isEventEmit EventEmit _ = True
isEventEmit _           = False

instance Show Reaction where
  show (ActionCall a) = a ++ "()"
  show (EventEmit e) = '^' : e
  showsPrec _ _ = id

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
