
module Sisyphus.SisSyn where


type Event  = String
type Action = String

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

data SisStateMachine a = SSM {
    ssmName :: String
  , ssmEvents :: [Event]
  , ssmActions :: [Action]
  , ssmRegions :: [SisRegion a]
  , ssmTransitions :: [SisTransition a]
}

data SisRegion a = SR {
    srName :: Maybe a
  , srVertices :: [SisState a]
  , srInitial :: Maybe a
  , srFinals :: [a]
}

data SisState a =
  SSKSimple {
    sskSimpleName :: a
  , sskSimpleIndex :: !Int
  , sskSimpleBehaviours :: [SisBehavior]
  }
  |
  SSKComposite {
    sskCompositeName :: a
  , sskCompositeIndex :: !Int
  , sskCompositeBehaviours :: [SisBehavior]
  , sskCompositeRegions :: [SisRegion a]
  }
  |
  SSKSubmachine
  |
  SSKPseudo SisPseudoState

data SisBehavior = SBEntry [SisEffect]
                 | SBExit [SisEffect]
                 | SBDoActivity [SisEffect]

data SisEffect = SE {
    seKind :: SisEffectKind
  , seName :: String
}

data SisEffectKind = SEKEvent
                   | SEKAction
                   | SEKDoCall

data SisPseudoState = SPSInitial
                    | SPSFinal
                    | SPSJunction
                    | SPSChoice
                    | SPSJoin
                    | SPSFork
                    | SPSShallowHistory
                    | SPSDeepHistory
                    | SPSEntryPoint
                    | SPSExitPoint

data SisTransition a =
  ST {
    stKind :: SisTransitionKind
  , stTriggers :: [String]
  , stGuard :: Guard
  , stBehaviors :: [SisBehavior]
  , stSrc :: a
  , stDst :: a
  }

data SisTransitionKind = STKInternal
                       | STKExternal
                       | STKLocal
