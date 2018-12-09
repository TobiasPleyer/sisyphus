module Sisyphus.Util where


import Data.Word
import Data.Bits
import Data.Char
import Data.Maybe
import Data.List ((\\), isPrefixOf)
import qualified Data.Map.Strict as M
import qualified Data.IntMap.Strict as IM
import qualified Data.Set as S

import Sisyphus.SisSyn


-- | Encode a Haskell String to a list of Word8 values, in UTF8 format.
encode :: Char -> [Word8]
encode = map fromIntegral . go . ord
 where
  go oc
   | oc <= 0x7f       = [oc]

   | oc <= 0x7ff      = [ 0xc0 + (oc `shiftR` 6)
                        , 0x80 + oc .&. 0x3f
                        ]

   | oc <= 0xffff     = [ 0xe0 + (oc `shiftR` 12)
                        , 0x80 + ((oc `shiftR` 6) .&. 0x3f)
                        , 0x80 + oc .&. 0x3f
                        ]
   | otherwise        = [ 0xf0 + (oc `shiftR` 18)
                        , 0x80 + ((oc `shiftR` 12) .&. 0x3f)
                        , 0x80 + ((oc `shiftR` 6) .&. 0x3f)
                        , 0x80 + oc .&. 0x3f
                        ]


dedupeList :: Ord a => [a] -> ([a],[a])
dedupeList items =
  let uniques = S.toList $ S.fromList items
      redefs = items \\ uniques
  in (redefs,uniques)


addIngoingTransition :: (SisTransition a) -> (SisState a) -> (SisState a)
addIngoingTransition t s =
  let
    ins = stnIngoingTransitions s
    s' = s{stnIngoingTransitions=(t:ins)}
  in s'


addOutgoingTransition :: (SisTransition a) -> (SisState a) -> (SisState a)
addOutgoingTransition t s =
  let
    outs = stnOutgoingTransitions s
    s' = s{stnOutgoingTransitions=(t:outs)}
  in s'


isTriggeredBy :: (SisState a) -> Event -> Bool
isTriggeredBy state event = not
                          . null
                          . filter (==event)
                          . map fromJust
                          . filter isJust
                          . concat
                          $ [ map stTrigger (stnOutgoingTransitions state)
                            , map stTrigger (stnInternalTransitions state)
                            ]


lookupStateFromRdrId :: M.Map String [([String],Int)] -> RdrId -> [Int]
lookupStateFromRdrId stateLookup rdrId =
  let (name,prefix) = case rdrId of
                        UnqualId id -> (id,[])
                        QualId ids -> (last ids, reverse (init ids))
      candidates = M.lookup name stateLookup
  in case candidates of
    Nothing -> []
    Just cs -> map snd (filter ((prefix `isPrefixOf`) . fst) cs)


hasPseudoStates :: SisTransition RdrId -> Bool
hasPseudoStates trans = any ($ trans) [isInitialTrans, isFinalTrans]


isInitialTrans :: SisTransition RdrId -> Bool
isInitialTrans trans = ((stSrc trans) == (UnqualId "[*]"))


isFinalTrans :: SisTransition RdrId -> Bool
isFinalTrans trans = ((stDst trans) == (UnqualId "[*]"))


isEntryReaction :: SisBehavior -> Bool
isEntryReaction (SBEntry _) = True
isEntryReaction _ = False


isExitReaction :: SisBehavior -> Bool
isExitReaction (SBExit _) = True
isExitReaction _ = False


isDoActivity :: SisBehavior -> Bool
isDoActivity (SBDoActivity _) = True
isDoActivity _ = False


getAllEntries :: [SisBehavior] -> [SisEffect]
getAllEntries = concat . map unwrap . filter isEntryReaction
    where unwrap (SBEntry es) = es


getAllExits :: [SisBehavior] -> [SisEffect]
getAllExits = concat . map unwrap . filter isExitReaction
    where unwrap (SBExit es) = es


getAllDoActivities :: [SisBehavior] -> [SisEffect]
getAllDoActivities = concat . map unwrap . filter isDoActivity
    where unwrap (SBDoActivity es) = es


getAllBehaviorEvents :: SisBehavior -> [String]
getAllBehaviorEvents (SBEntry es) = getAllEventsFromEffects es
getAllBehaviorEvents (SBExit es) = getAllEventsFromEffects es
getAllBehaviorEvents (SBDoActivity es) = getAllEventsFromEffects es


getAllBehaviorActions :: SisBehavior -> [String]
getAllBehaviorActions (SBEntry as) = getAllActionsFromEffects as
getAllBehaviorActions (SBExit as) = getAllActionsFromEffects as
getAllBehaviorActions (SBDoActivity as) = getAllActionsFromEffects as


getAllEventsFromEffects :: [SisEffect] -> [String]
getAllEventsFromEffects es = map seName $ filter ((==SEKEvent) . seKind) es


getAllActionsFromEffects :: [SisEffect] -> [String]
getAllActionsFromEffects es = map seName $ filter ((==SEKAction) . seKind) es


isInternalTransition :: SisTransition a -> Bool
isInternalTransition t = (stKind t) == STKInternal


isExternalTransition :: SisTransition a -> Bool
isExternalTransition t = (stKind t) == STKExternal


getAllInternalTransitions :: [SisTransition a] -> [SisTransition a]
getAllInternalTransitions = filter isInternalTransition


getAllExternalTransitions :: [SisTransition a] -> [SisTransition a]
getAllExternalTransitions = filter isInternalTransition