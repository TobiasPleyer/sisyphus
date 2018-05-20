module Sisyphus.Types where


data RawStateMachine = RSM
  { rsmEvents :: [Event]
  , rsmActions :: [Action]
  , rsmStates :: [State]
  , rsmTransitions :: [Transition]
  } deriving (Show)

type Event  = String
type Action = String
type Guard  = String

data Transition = Trans
  { trSrc :: String
  , trDest :: String
  , trReacts :: [ReactionSpec]
  } deriving (Show)

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

data Reaction = ActionCall Action
              | EventEmit Event
              deriving (Show)

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
