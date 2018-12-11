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
import qualified Data.Array as A
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import System.FilePath
import Text.Ginger
import Sisyphus.SisSyn
import Sisyphus.Util
import Sisyphus.Language.Template


renderCSimple :: StateMachine -> FilePath -> IO ()
renderCSimple sm outDir = do
    renderHeader sm outDir
    renderSource sm outDir

renderHeader :: StateMachine -> FilePath -> IO ()
renderHeader sm@SM{..} outDir = do
    let
        target = smName <.> "h"
        cHeaderTmpl = "C/fsm.h.tmpl"
        context = makeContextText (
            (M.!) (M.fromList [("FSM_NAME", toGVal smName)
                              ,("FSM_EVENTS", toGVal $ A.elems smEvents)
                              ,("FSM_ACTIONS", toGVal $ A.elems smActions)
                              ,("FSM_STATES", toGVal $ map stnName (A.elems smStates))
                              ]
                  )
            )
    renderFromFile context outDir cHeaderTmpl target


renderSource :: StateMachine -> FilePath -> IO ()
renderSource sm@SM{..} outDir = return ()