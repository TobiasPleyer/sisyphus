{-# LANGUAGE OverloadedStrings, FlexibleInstances, MultiParamTypeClasses, RecordWildCards #-}
module Sisyphus.Ginger where


import Data.Default
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Text.Ginger.GVal
import Sisyphus.Types


instance ToGVal m Reaction where
  toGVal (ActionCall action) = toGVal action
  toGVal (EventEmit event) = toGVal event


instance ToGVal m Guard where
  toGVal g = def


instance ToGVal m ReactionSpec where
  toGVal rs = def
              { asBoolean = True
              , isNull = False
              , asLookup = Just (\t -> case t of
                                       "trigger"   -> Just gValTrigger
                                       "guards"    -> Just gValGuards
                                       "reactions" -> Just gValReactions
                                       _           -> Nothing
                                  )
              }
    where
      gValTrigger = toGVal $ maybe "" (T.pack) (rspecTrigger rs)
      gValGuards = toGVal (rspecGuards rs)
      gValReactions = toGVal (rspecReactions rs)


instance ToGVal m TransitionSpec where
  toGVal ts = def
              { asBoolean = True
              , isNull = False
              , asLookup = Just (\t -> case t of
                                       "trigger"   -> Just gValTrigger
                                       "guards"    -> Just gValGuards
                                       "reactions" -> Just gValReactions
                                       _           -> Nothing
                                  )
              }
    where
      gValSrc = toGVal $ T.pack $ tspecSrc ts
      gValDst = toGVal $ T.pack $ tspecDst ts
      gValTrigger = toGVal $ maybe "" (T.pack) (tspecTrigger ts)
      gValGuards = toGVal (tspecGuards ts)
      gValReactions = toGVal (tspecReactions ts)


instance ToGVal m State where
  toGVal s = def
             { asBoolean = True
             , isNull = False
             , asLookup = Just (\t -> case t of
                                      "name"         -> Just gValName
                                      "entries"      -> Just gValEntries
                                      "exits"        -> Just gValExits
                                      "internals"    -> Just gValInternals
                                      "ingoings"     -> Just gValIngoings
                                      "outgoings"    -> Just gValOutgoings
                                      "hasEntries"   -> Just gValHasEntries
                                      "hasExits"     -> Just gValHasExits
                                      "hasInternals" -> Just gValHasInternals
                                      "entryEmits"   -> Just gValEntryEmits
                                      "exitEmits"    -> Just gValExitEmits
                                      "entryActions" -> Just gValEntryActions
                                      "exitActions"  -> Just gValExitActions
                                      _              -> Nothing
                                 )
             }
    where
      gValName = toGVal $ T.pack $ stName s
      gValEntries = toGVal $ stEntryReactions s
      gValExits = toGVal $ stExitReactions s
      gValInternals = toGVal $ stInternalReactions s
      gValIngoings = toGVal $ stIngoingTransitions s
      gValOutgoings = toGVal $ stOutgoingTransitions s
      gValHasEntries = toGVal (not (null (stEntryReactions s)))
      gValHasExits = toGVal (not (null (stExitReactions s)))
      gValHasInternals = toGVal (not (null (stInternalReactions s)))
      gValEntryEmits = toGVal
                       . filter isEventEmit
                       . concat
                       . map rspecReactions
                       . stEntryReactions
                       $ s
      gValExitEmits = toGVal
                      . filter isEventEmit
                      . concat
                      . map rspecReactions
                      . stExitReactions
                      $ s
      gValEntryActions = toGVal
                         . filter isActionCall
                         . concat
                         . map rspecReactions
                         . stEntryReactions
                         $ s
      gValExitActions = toGVal
                        . filter isActionCall
                        . concat
                        . map rspecReactions
                        . stExitReactions
                        $ s


instance ToGVal m v => ToGVal m (M.Map String v) where
  toGVal xs = def
                { asBoolean = not . M.null $ xs'
                , isNull = False
                , asLookup = Just (`M.lookup` xs')
                , asDictItems = Just $ M.toList xs'
                }
    where
      xs' = M.map toGVal $ M.mapKeys T.pack xs


instance ToGVal m StateMachine where
  toGVal sm = def
            { asBoolean = True
            , isNull = False
            , asLookup = Just (\t -> case t of
                                     "name"        -> Just gValName
                                     "events"      -> Just gValEvents
                                     "actions"     -> Just gValActions
                                     "states"      -> Just gValStates
                                     "transitions" -> Just gValTransitions
                                     _             -> Nothing
                                )
            }
    where
      gValName = toGVal $ T.pack $ smName sm
      gValEvents = toGVal $ smEvents sm
      gValActions = toGVal $ smActions sm
      gValStates = toGVal $ smStates sm
      gValTransitions = toGVal $ smTransitions sm
