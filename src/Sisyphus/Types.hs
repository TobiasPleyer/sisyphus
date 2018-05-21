module Sisyphus.Types where


data RawStateMachine = RSM
  { rsmEvents :: [Event]
  , rsmActions :: [Action]
  , rsmStates :: [State]
  , rsmTransitions :: [TransitionSpec]
  } deriving (Show)

type Event  = String
type Action = String
type Guard  = String

data State = State
  { stName              :: String
  , stEntryReactions    :: [ReactionSpec]
  , stExitReactions     :: [ReactionSpec]
  , stInternalReactions :: [ReactionSpec]
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
