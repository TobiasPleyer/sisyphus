module Sisyphus.Types where


data RawStateMachine = RSM
  { rsmEvents :: [Event]
  , rsmActions :: [Action]
  , rsmStates :: [State]
  , rsmTransitions :: [Transition]
  } deriving (Show)

type Event  = String
type Action = String

data Transition = Trans
  { trSrc :: String
  , trDest :: String
  , trReacts :: [Reaction]
  } deriving (Show)

data State = State
  { stName              :: String
  , stEntryReactions    :: [Reaction]
  , stExitReactions     :: [Reaction]
  , stInternalReactions :: [Reaction]
  } deriving (Show)

data Reaction = ActionCall Action
              | EventEmit Event
              deriving (Show)
