{-# LANGUAGE RecordWildCards #-}

module Sisyphus.SisSyn where

import Data.List (intersperse)
import qualified Data.Map.Strict as M
import qualified Data.Array as A


type Id = Int
type Event  = String
type Action = String


data RdrDecl = StateDecl String [[RdrDecl]]
             | BehaviorDecl RdrId SisBehavior
             | TransDecl (SisTransition RdrId)
             deriving (Show)

isStateDecl :: RdrDecl -> Bool
isStateDecl (StateDecl _ _) = True
isStateDecl _ = False

isBehaviorDecl :: RdrDecl -> Bool
isBehaviorDecl (BehaviorDecl _ _) = True
isBehaviorDecl _ = False

isTransDecl :: RdrDecl -> Bool
isTransDecl (TransDecl _) = True
isTransDecl _ = False

data RdrId = UnqualId String
           | QualId [String]
           deriving(Eq)

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

data StateMachine = SM {
    smName :: String
  , smEvents :: [String]
  , smActions :: [String]
  , smStateArray :: A.Array Int (SisState Id)
  , smRegionArray :: A.Array Int (SisRegion Id)
  , smTopRegions :: [Id]
  , smTransitions :: [SisTransition Id]
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
  , stnInternalTransitions :: [SisTransition a]
  , stnIngoingTransitions  :: [SisTransition a]
  , stnOutgoingTransitions :: [SisTransition a]
  }
  |
  STSubmachine
  |
  STPseudo SisPseudoState
  deriving (Show)

data SisBehavior = SBEntry [SisEffect]
                 | SBExit [SisEffect]
                 | SBDoActivity [SisEffect]
                 deriving (Eq,Show)

data SisEffect = SE {
    seKind :: SisEffectKind
  , seName :: String
  }
  deriving (Eq,Show)

data SisEffectKind = SEKEvent
                   | SEKAction
                   | SEKDoCall
                   deriving (Eq,Show)

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
                    deriving (Eq,Show)

data SisTransition a =
  ST {
    stKind :: SisTransitionKind
  , stTrigger :: Maybe String
  , stGuard :: Maybe Guard
  , stEffects :: [SisEffect]
  , stSrc :: a
  , stDst :: a
  }
  deriving (Show)

data SisTransitionKind = STKInternal
                       | STKExternal
                       | STKLocal
                       deriving (Eq,Show)
