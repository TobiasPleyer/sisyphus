module Types where


data RawStateMachine = RSM
  { rsmEvents :: [Event]
  , rsmActions :: [Action]
  , rsmStates :: [State]
  , rsmTransitions :: [Transition]
  }

type Event  = String
type Action = String
type Transition = String

data State = State
  { stName              :: String
  , stEntryReactions    :: [Reaction]
  , stExitReactions     :: [Reaction]
  , stInternalReactions :: [Reaction]
  }

data Reaction = ActionCall Action
              | EventEmit Event
