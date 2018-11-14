
module Sisyphus.SisSyn where

import Data.List (intersperse)


data RdrDecl = StateDecl String [[RdrDecl]]
             | BehaviorDecl RdrId SisBehavior
             | TransDecl (SisTransition RdrId)
             deriving (Show)

data RdrId = UnqualId String
           | QualId [String]

instance Show RdrId where
  show (UnqualId id) = id
  show (QualId ids) = concat $ intersperse "." ids
  showsPrec _ r s = (show r) ++ s

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
  , ssmRegions :: [a]
  , ssmTransitions :: [SisTransition a]
  }
  deriving (Show)

data SisRegion a = SR {
    srName :: String
  , srIndex :: !Int
  , srVertices :: [a]
  , srInitial :: Maybe a
  , srFinals :: [a]
  }
  deriving (Show)

data SisState a =
  STNormal {
    stnName :: String
  , stnId :: a
  , stnIndex :: a
  , stnBehaviors :: [SisBehavior]
  , stnRegions :: [a]
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
