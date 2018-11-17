module Sisyphus.Util where


import Data.Word
import Data.Bits
import Data.Char
import Data.Maybe
import Data.List ((\\), isPrefixOf)
import qualified Data.Map.Strict as M
import qualified Data.IntMap.Strict as IM
import qualified Data.Set as S

import Sisyphus.Types
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


addIngoingTransition :: TransitionSpec -> State -> State
addIngoingTransition t s =
  let
    ins = stIngoingTransitions s
    s' = s{stIngoingTransitions=(t:ins)}
  in s'


addOutgoingTransition :: TransitionSpec -> State -> State
addOutgoingTransition t s =
  let
    outs = stOutgoingTransitions s
    s' = s{stOutgoingTransitions=(t:outs)}
  in s'


isTriggeredBy :: State -> Event -> Bool
isTriggeredBy state event = not
                          . null
                          . filter (==event)
                          . map fromJust
                          . filter isJust
                          . concat
                          $ [ map tspecTrigger (stOutgoingTransitions state)
                            , map rspecTrigger (stInternalReactions state)
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
hasPseudoStates trans = ((stSrc trans) == (UnqualId "[*]")) || ((stDst trans) == (UnqualId "[*]"))


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