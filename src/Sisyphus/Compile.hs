{-# LANGUAGE RecordWildCards #-}

module Sisyphus.Compile where


import Control.Monad (forM, forM_, mapM_, when)
import Data.Foldable (traverse_)
import qualified Control.Monad.Trans.State.Lazy as TSL
import qualified Data.Array as A
import Data.Foldable (foldr')
import qualified Data.Map.Strict as M
import qualified Data.IntMap.Strict as IM
import qualified Data.Set as S
import Data.Maybe (isJust)
import System.Exit (exitFailure, exitSuccess, exitWith, ExitCode(..))
import System.IO (stderr, hPutStr)
import System.FilePath
import Text.Show.Pretty (pPrint)

import Sisyphus.SisSyn
import Sisyphus.Lexer
import Sisyphus.Parser
import Sisyphus.Util


bye :: String -> IO a
bye s = putStr s >> exitWith ExitSuccess

die :: String -> IO a
die s = hPutStr stderr s >> exitWith (ExitFailure 1)


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

compile :: Bool
        -- ^ should warnings be treated as errors?
        -> Bool
        -- ^ should warnings be ignored
        -> FilePath
        -- ^ The file containing the state machine definition
        -> IO StateMachine
        -- ^ the final result should be a state machine in the IO Monad (or die)
compile warnEqErr ignoreWarn file = do
  let debug = True
  str <- readFile file
  decls <- case runP str parse of
    Left (Just (AlexPn _ line col),err) ->
            die (file ++ ":" ++ show line ++ ":" ++ show col
                             ++ ": " ++ err ++ "\n")
    Left (Nothing, err) ->
            die (file ++ ": " ++ err ++ "\n")

    Right (_,decls) -> return decls
  when debug $ do
    putStrLn "Declarations"
    pPrint decls
    putStrLn "------"
  let
    summary = runDecls decls
    warnings = gsWarnings summary
    errors = gsErrors summary
    (ws,es) = if warnEqErr then ([],warnings ++ errors) else (warnings,errors)
  when ((not $ null $ ws) && (not ignoreWarn)) $ do
    putStrLn "== Warnings =="
    mapM_ putStrLn ws
  when (not $ null $ es) $ do
    putStrLn "The following errors prevent further processing:"
    putStrLn "== Errors =="
    mapM_ putStrLn es
    exitFailure
  let
    regionArray = (A.array (0, gsRegionIndex summary) $ IM.toList $ gsRegionMap summary)
    stateMap = connectTransitions (gsTransitions summary) (gsStateMap summary)
    stateArray = (A.array (0, gsStateIndex summary) $ IM.toList $ stateMap)
    stateMachine = SM
                    "SM"
                    (S.toList $ gsEvents summary)
                    (S.toList $ gsActions summary)
                    stateArray
                    regionArray
                    (gsRegions summary)
                    (gsTransitions summary)
  when debug $ do
    putStrLn "State Machine"
    pPrint stateMachine
    putStrLn "------"
  return stateMachine

runDecls :: [RdrDecl] -> GrammarSummary
runDecls decls = TSL.execState (
    do
        toplevelRegions <- runRegionDeclsM [decls]
        TSL.modify (\st -> st{gsRegions=(map srIndex toplevelRegions)})
    ) initialSummary

connectTransitions :: [SisTransition Id] -> (IM.IntMap (SisState Id)) -> (IM.IntMap (SisState Id))
connectTransitions ts ss = foldr' addInAndOutgoing ss ts where
    addInAndOutgoing t@ST{..} ss = case stKind of
        STKInternal -> IM.update (Just . addInternalTransition t) stSrc ss
        STKExternal -> ( (IM.update (Just . addOutgoingTransition t) stSrc)
                       . (IM.update (Just . addIngoingTransition t) stDst)
                       ) ss
        STKLocal -> ss

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
        addNormalStateM name id rIndex [] (map srIndex regionIds)

runBehaviorDeclsM :: [RdrDecl] -> SummaryM ()
runBehaviorDeclsM decls = do
    let behaviorDecls = filter isBehaviorDecl decls
    stateLookup <- TSL.gets gsStateLookup
    stateMap <- TSL.gets gsStateMap
    forM_ behaviorDecls $ \(BehaviorDecl rdrId behavior) -> do
        ifUniqueStateId rdrId $ \id -> do
            addBehaviorToStateM behavior id
            let events = getAllBehaviorEvents behavior
                actions = getAllBehaviorActions behavior
            TSL.modify $ \st -> st{
                gsEvents = S.union (gsEvents st) (S.fromList events),
                gsActions = S.union (gsActions st) (S.fromList actions)
                }

runTransitionDeclsM :: [RdrDecl] -> SummaryM ()
runTransitionDeclsM decls = do
    let transitionDecls = filter isTransDecl decls
    forM_ transitionDecls $ \(TransDecl trans) -> do
        if (hasPseudoStates trans)
        then runPseudoTransitionM trans
        else runNormalTransitionM trans

runNormalTransitionM :: (SisTransition RdrId) -> SummaryM ()
runNormalTransitionM trans = do
    let srcRdrId = stSrc trans
        dstRdrId = stDst trans
    ifUniqueStateId srcRdrId $ \srcId ->
        ifUniqueStateId dstRdrId $ \dstId -> do
            addTransition (trans{stSrc=srcId, stDst=dstId})
            let effectEvents = getAllEventsFromEffects (stEffects trans)
                effectActions = getAllActionsFromEffects (stEffects trans)
                triggerEvent = stTrigger trans
            TSL.modify $ \st -> st{
                gsEvents = S.union (gsEvents st) (S.fromList (maybe effectEvents (: effectEvents) triggerEvent)),
                gsActions = S.union (gsActions st) (S.fromList effectActions)
                }

runPseudoTransitionM :: (SisTransition RdrId) -> SummaryM ()
runPseudoTransitionM trans = do
    if (isInitialTrans trans)
    then ifUniqueStateId (stDst trans) markStateInitialM
    else if (isFinalTrans trans)
         then ifUniqueStateId (stSrc trans) markStateFinalM
         else addError $ "Unknown pseudo state in transition " ++ (show trans)

runRegionDeclsM :: [[RdrDecl]] -> SummaryM [SisRegion Id]
runRegionDeclsM declss = do
    forM declss $ \decls -> do
        id <- newRegionId
        addEmptyRegion "" id
        vertices <- runDeclsM decls
        regionMap <- getRegionMap
        let region = (IM.!) regionMap id
            region' = region{ srVertices=(map stnId vertices) }
            regionMap' = IM.insert id region' regionMap
        TSL.modify (\st -> st{ gsRegionMap=regionMap' })
        return region'

ifUniqueStateId :: RdrId -> (Id -> SummaryM ()) -> SummaryM ()
ifUniqueStateId rdrId action = do
    stateLookup <- TSL.gets gsStateLookup
    let stateIds = lookupStateFromRdrId stateLookup rdrId
    if (null stateIds)
    then addError $ "Unknown state: " ++ (show rdrId)
    else if (length stateIds > 1)
            then addError $ "State '" ++ (show rdrId) ++ "' is ambiguous"
            else action (head stateIds)

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

getWarnings :: SummaryM [String]
getWarnings = TSL.gets gsWarnings

setWarnings :: [String] -> SummaryM ()
setWarnings ws = TSL.state (\gs -> ((), gs{gsWarnings=ws}))

addWarning :: String -> SummaryM ()
addWarning w = TSL.state (\gs -> ((), gs{gsWarnings=w:(gsWarnings gs)}))

getErrors :: SummaryM [String]
getErrors = TSL.gets gsErrors

setErrors :: [String] -> SummaryM ()
setErrors es = TSL.state (\gs -> ((), gs{gsErrors=es}))

addError :: String -> SummaryM ()
addError e = TSL.state (\gs -> ((), gs{gsErrors=e:(gsErrors gs)}))

getStateMap :: SummaryM (IM.IntMap (SisState Id))
getStateMap = TSL.gets gsStateMap

getRegionMap :: SummaryM (IM.IntMap (SisRegion Id))
getRegionMap = TSL.gets gsRegionMap

addNormalStateM :: String -> Id -> Id -> [SisBehavior] -> [Id] -> SummaryM (SisState Id)
addNormalStateM name id index behaviors regions = do
    scope <- getScope
    stateLookup <- TSL.gets gsStateLookup
    stateMap <- TSL.gets gsStateMap
    let
        state = STNormal name id index behaviors regions [] [] []
        stateMap' = IM.insert id state stateMap
        stateLookup' = M.alter (\ms -> case ms of
                                  Nothing -> Just [(scope,id)]
                                  Just ss -> Just ((scope,id):ss)) name stateLookup
    TSL.modify (\st -> st{ gsStateMap=stateMap'
                         , gsStateLookup=stateLookup' })
    return state

addRegionM :: String -> Id -> [Id] -> (Maybe Id) -> [Id] -> SummaryM (SisRegion Id)
addRegionM name id vertices initial finals = do
    regionMap <- getRegionMap
    let region = SR name id vertices initial finals
        regionMap' = IM.insert id region regionMap
    TSL.modify (\st -> st{ gsRegionMap=regionMap' })
    return region

addEmptyRegion :: String -> Id -> SummaryM (SisRegion Id)
addEmptyRegion name id = addRegionM name id [] Nothing []

addBehaviorToStateM :: SisBehavior -> Id -> SummaryM ()
addBehaviorToStateM behavior stateId = do
    stateMap <- TSL.gets gsStateMap
    let state = (IM.!) stateMap stateId
        state' = state{stnBehaviors=(behavior:stnBehaviors state)} 
        stateMap' = IM.insert stateId state' stateMap
    TSL.modify (\st -> st{ gsStateMap=stateMap' })

addTransition :: SisTransition Id -> SummaryM ()
addTransition t = TSL.state (\gs -> ((), gs{gsTransitions=t:(gsTransitions gs)}))

markStateInitialM :: Id -> SummaryM ()
markStateInitialM stateId = do
    stateMap <- getStateMap
    regionMap <- getRegionMap
    let state = (IM.!) stateMap stateId
        regionIndex = stnIndex state
        region = (IM.!) regionMap regionIndex
    if (isJust (srInitial region))
    then addError $ "Region of state " ++ (stnName state) ++ " already has an intial state"
    else do
         let region' = region{ srInitial = (Just stateId) }
             regionMap' = IM.insert regionIndex region' regionMap
         TSL.modify (\st -> st{ gsRegionMap=regionMap' })

markStateFinalM :: Id -> SummaryM ()
markStateFinalM stateId = do
    stateMap <- getStateMap
    regionMap <- getRegionMap
    let state = (IM.!) stateMap stateId
        regionIndex = stnIndex state
        region = (IM.!) regionMap regionIndex
    if (stateId `elem` (srFinals region))
    then addError $ "State " ++ (stnName state) ++ " has been marked final more than once"
    else do
         let region' = region{ srFinals = stateId:(srFinals region) }
             regionMap' = IM.insert regionIndex region' regionMap
         TSL.modify (\st -> st{ gsRegionMap=regionMap' })