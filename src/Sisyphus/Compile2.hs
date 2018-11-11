{-# LANGUAGE RecordWildCards #-}

module Sisyphus.Compile2 where


import Control.Monad (forM_, mapM_)
import Data.Foldable (traverse_)
import qualified Control.Monad.Trans.State.Lazy as TSL
import qualified Data.Map.Strict as M
import qualified Data.IntMap.Strict as IM
import qualified Data.Set as S

import Sisyphus.SisSyn
import Sisyphus.Lexer
import Sisyphus.Parser


type Id = Int

data GrammarSummary = GS
  { gsStateIndex :: !Int
  , gsRegionIndex :: !Int
  , gsScope :: [String]
  , gsEvents :: S.Set String
  , gsActions :: S.Set String
  , gsRegions :: [Id]
  , gsTransitions :: [SisTransition Id]
  , gsStateLookup :: M.Map String [([String],Id)]
  , gsStateMap :: IM.IntMap (SisState Id)
  , gsWarnings :: [String]
  , gsErrors :: [String]
  } deriving (Show)

type SummaryM = TSL.State GrammarSummary

initialSummary :: GrammarSummary
initialSummary = GS 0 0 [] S.empty S.empty [] [] M.empty IM.empty [] []

runDecls :: [RdrDecl] -> GrammarSummary
runDecls decls = TSL.execState (runDeclsM decls) initialSummary

runDeclsM :: [RdrDecl] -> SummaryM ()
runDeclsM decls = do
    runStateDecls decls
    runBehaviorDecls decls
    runTransitionDecls decls

runStateDecls :: [RdrDecl] -> SummaryM ()
runStateDecls decls = do
    let stateDecls = filter isStateDecl decls
    forM_ stateDecls (\(StateDecl name declss) -> do
    pushScope name
    regionIds <- runRegionDecls declss
    popScope
    addNormalState name [] regionIds
    )

runBehaviorDecls = undefined
runTransitionDecls = undefined
runRegionDecls = undefined

getScope :: SummaryM [String]
getScope = TSL.gets gsScope

setScope :: [String] -> SummaryM ()
setScope scope = TSL.modify (\st -> st{gsScope=scope})

pushScope :: String -> SummaryM ()
pushScope name = TSL.modify (\st -> st{gsScope=(name:gsScope st)})

popScope :: SummaryM String
popScope = do
    scope <- getScope
    case scope of
        (s:ss) -> setScope ss >> return s
        _      -> error "Empty stack!"

newStateId :: SummaryM Id
newStateId = do
    id <- TSL.gets gsStateIndex
    TSL.modify (\st -> st{gsStateIndex=id+1})
    return id

newRegionId :: SummaryM Id
newRegionId = do
    id <- TSL.gets gsRegionIndex
    TSL.modify (\st -> st{gsRegionIndex=id+1})
    return id

lookupState :: String -> SummaryM ([(Scope,Id)])
lookupState s = (M.findWithDefault [] s) <$> (TSL.gets gsStateLookup)

addNormalState :: String -> [SisBehavior] -> [Id] -> SummaryM ()
addNormalState name behaviors regions = do
    id <- newStateId
    let
        name = stnName state
        state = STNormal name id behaviors regions
    scope <- getScope
    stateLookup <- TSL.gets gsStateLookup
    stateMap <- TSL.gets gsStateMap
    let
        stateMap' = IM.insert id state stateMap
        stateLookup' = M.alter (\ms -> case ms of
                                  Nothing -> Just [(scope,id)]
                                  Just ss -> Just ((scope,id):ss)) name stateLookup
    TSL.modify (\st -> st{ gsStateMap=stateMap'
                         , gsStateLookup=stateLookup' })