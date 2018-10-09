{-# LANGUAGE PatternGuards, FlexibleInstances, MultiParamTypeClasses #-}
module Sisyphus.Ginger where


import qualified Data.HashMap.Strict as M
import Text.Ginger.GVal
import Sisyphus.Types


instance ToGVal m Reaction where
  toGVal (ActionCall action) = toGVal action
  toGVal (EventEmit event) = toGVal event

instance ToGVal m v => ToGVal m (M.HashMap String v) where
  toGVal m = ( M.fromList
             . map (\(k,v) -> (toGVal (T.pack k),toGVal v))
             . M.toList) m
