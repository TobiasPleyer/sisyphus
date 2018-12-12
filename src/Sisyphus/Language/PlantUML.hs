{-# LANGUAGE OverloadedStrings #-}

module Sisyphus.Language.PlantUML

where


import qualified Data.Map.Strict as M
import qualified Data.Text as T
import System.FilePath
import System.Exit (ExitCode(..), exitWith)
import Text.Ginger
       (makeContextText, Template, toGVal, runGinger, parseGingerFile, VarName)
import Text.Ginger.GVal (ToGVal, GVal)
import Sisyphus.SisSyn
import Sisyphus.Language.Template


gvTemplateSimple = "PlantUML/fsm.plant.tmpl"


renderPlantUML :: TemplateLoader -> StateMachine -> FilePath -> IO ()
renderPlantUML templateLoader sm outDir = undefined