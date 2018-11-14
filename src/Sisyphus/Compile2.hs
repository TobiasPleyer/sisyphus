{-# LANGUAGE RecordWildCards #-}

module Sisyphus.Compile2 where


import Control.Monad (forM, forM_, mapM_)
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
  , gsRegionMap :: IM.IntMap (SisRegion Id)
  , gsWarnings :: [String]
  , gsErrors :: [String]
  } deriving (Show)

type SummaryM = TSL.State GrammarSummary

initialSummary :: GrammarSummary
initialSummary = GS (-1) (-1) [] S.empty S.empty [] [] M.empty IM.empty IM.empty [] []

runDecls :: [RdrDecl] -> GrammarSummary
runDecls decls = TSL.execState (runRegionDeclsM [decls]) initialSummary

runDeclsM :: [RdrDecl] -> SummaryM [SisState Id]
runDeclsM decls = do
    rIndex <- currentRegionIndex
    states <- runStateDeclsM rIndex decls
    runBehaviorDeclsM decls
    runTransitionDeclsM decls
    return states

runStateDeclsM :: Id -> [RdrDecl] -> SummaryM [SisState Id]
runStateDeclsM rIndex decls = do
    let stateDecls = filter isStateDecl decls
    forM stateDecls $ \(StateDecl name declss) -> do
        id <- newStateId
        pushScope name
        regionIds <- runRegionDeclsM declss
        popScope
        addNormalState name id rIndex [] (map srIndex regionIds)

runBehaviorDeclsM _ = return ()
runTransitionDeclsM _ = return ()

runRegionDeclsM :: [[RdrDecl]] -> SummaryM [SisRegion Id]
runRegionDeclsM declss = do
    forM declss $ \decls -> do
        id <- newRegionId
        vertices <- runDeclsM decls
        addRegion "" id (map stnId vertices) Nothing []

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
    new_id <- (+1) <$> TSL.gets gsStateIndex
    TSL.modify (\st -> st{gsStateIndex=new_id})
    return new_id

newRegionId :: SummaryM Id
newRegionId = do
    new_id <- (+1) <$> TSL.gets gsRegionIndex
    TSL.modify (\st -> st{gsRegionIndex=new_id})
    return new_id

currentRegionIndex = TSL.gets gsRegionIndex

lookupState :: String -> SummaryM ([(Scope,Id)])
lookupState s = (M.findWithDefault [] s) <$> (TSL.gets gsStateLookup)

addNormalState :: String -> Id -> Id -> [SisBehavior] -> [Id] -> SummaryM (SisState Id)
addNormalState name id index behaviors regions = do
    scope <- getScope
    stateLookup <- TSL.gets gsStateLookup
    stateMap <- TSL.gets gsStateMap
    let
        state = STNormal name id index behaviors regions
        stateMap' = IM.insert id state stateMap
        stateLookup' = M.alter (\ms -> case ms of
                                  Nothing -> Just [(scope,id)]
                                  Just ss -> Just ((scope,id):ss)) name stateLookup
    TSL.modify (\st -> st{ gsStateMap=stateMap'
                         , gsStateLookup=stateLookup' })
    return state

addRegion :: String -> Id -> [Id] -> (Maybe Id) -> [Id] -> SummaryM (SisRegion Id)
addRegion name id vertices initial finals = do
    regionMap <- TSL.gets gsRegionMap
    let region = SR name id vertices initial finals
        regionMap' = IM.insert id region regionMap
    TSL.modify (\st -> st{ gsRegionMap=regionMap'})
    return region