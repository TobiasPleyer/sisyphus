{-# LANGUAGE RecordWildCards #-}

module Sisyphus.Types where


import qualified Data.Map.Strict as M
import qualified Data.IntMap.Strict as IM
import qualified Data.Set as S


data StateMachine = SM
  { smName        :: String
  , smStartState  :: String
  , smFinalStates :: [String]
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

data State = State
  { stName                :: String
  , stAttributes          :: [StateAttribute]
  , stEntryReactions      :: [ReactionSpec]
  , stExitReactions       :: [ReactionSpec]
  , stInternalReactions   :: [ReactionSpec]
  , stIngoingTransitions  :: [TransitionSpec]
  , stOutgoingTransitions :: [TransitionSpec]
  }

instance Show State where
  show State{..} = (init . unlines) $ stName : (concat [strAttrs, strEntries, strExits, strInternals, strIngoings, strOutgoings])
    where
      strAttrs = map (("  attributes: " ++) . show) stAttributes
      strEntries = map (("  entry: " ++) . show) stEntryReactions
      strExits = map (("  exit: " ++) . show) stExitReactions
      strInternals = map (("  internal: " ++) . show) stInternalReactions
      strIngoings = map (("  in: " ++) . show) stIngoingTransitions
      strOutgoings = map (("  out: " ++) . show) stOutgoingTransitions
  showsPrec _ r s = (show r) ++ s

type Event  = String
type Action = String
type TFuncSpec = [Reaction]

data Guard = G String
           | NotG String
           | BinOpG Operator Var Var
           deriving (Show)
data Var = V String
         | C String
         deriving (Show)
data Operator = OpEQ
              | OpNEQ
              | OpLT
              | OpGT
              | OpLE
              | OpGE
              deriving (Show)

data StateAttribute = StAInitial
                    | StAFinal
                    | ReactEntry [Reaction]
                    | ReactExit [Reaction]
                    | ReactInternal Event [Guard] [Reaction]
                    deriving (Show)

isInitial :: StateAttribute -> Bool
isInitial StAInitial = True
isInitial _          = False

isFinal :: StateAttribute -> Bool
isFinal StAFinal = True
isFinal _        = False

isEntry :: StateAttribute -> Bool
isEntry (ReactEntry _) = True
isEntry _              = False

isExit :: StateAttribute -> Bool
isExit (ReactExit _) = True
isExit _             = False

isInternal :: StateAttribute -> Bool
isInternal (ReactInternal _ _ _) = True
isInternal _                     = False

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
