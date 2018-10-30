
module Sisyphus.SisSyn where


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
  , ssmEvents :: [String]
  , ssmActions :: [String]
  , ssmRegions :: [SisRegion a]
  , ssmTransitions :: [SisTransition a]
  }
  deriving (Show)

data SisRegion a = SR {
    srName :: Maybe a
  , srIndex :: !Int
  , srVertices :: [SisState a]
  , srInitial :: Maybe a
  , srFinals :: [a]
  }
  deriving (Show)

data SisState a =
  STNormal {
    stnName :: a
  , stnIndex :: !Int
  , stnBehaviours :: [SisBehavior]
  , stnRegions :: [SisRegion a]
  }
  |
  STSubmachine
  |
  STPseudo SisPseudoState
  deriving (Show)

data SisBehavior = SBEntry [SisEffect]
                 | SBExit [SisEffect]
                 | SBDoActivity [SisEffect]
                 deriving (Show)

data SisEffect = SE {
    seKind :: SisEffectKind
  , seName :: String
  }
  deriving (Show)

data SisEffectKind = SEKEvent
                   | SEKAction
                   | SEKDoCall
                   deriving (Show)

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
                    deriving (Show)

data SisTransition a =
  ST {
    stKind :: SisTransitionKind
  , stTriggers :: [String]
  , stGuard :: Maybe Guard
  , stBehaviors :: [SisEffect]
  , stSrc :: a
  , stDst :: a
  }
  deriving (Show)

data SisTransitionKind = STKInternal
                       | STKExternal
                       | STKLocal
                       deriving (Eq,Show)
