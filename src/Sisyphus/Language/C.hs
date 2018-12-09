{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Sisyphus.Language.C
( renderCSimple
) where


import Conduit
import Control.Applicative (liftA2)
import Control.Monad (forM_)
import Data.Conduit
import Data.Monoid
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import System.FilePath
import Sisyphus.SisSyn
import Sisyphus.Util
import Sisyphus.Language.Template


renderCSimple :: StateMachine -> FilePath -> IO ()
renderCSimple sm outDir = undefined