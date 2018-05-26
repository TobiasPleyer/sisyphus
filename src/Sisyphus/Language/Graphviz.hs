{-# LANGUAGE OverloadedStrings #-}

module Sisyphus.Language.Graphviz
( renderGvSimple
) where


import qualified Data.Map as M
import qualified Data.Text as T
import System.Exit (ExitCode(..), exitWith)
import Text.Ginger
       (makeContextText, Template, toGVal, runGinger, parseGingerFile, VarName)
import Text.Ginger.GVal (ToGVal, GVal)
import Sisyphus.Types (RawStateMachine(..), TransitionSpec(..))
import Sisyphus.Language.Template (defaultTemplateLoader, renderTemplate)


gvTemplateSimple = "Graphviz/fsm.gv.tmpl"


gvTransition :: TransitionSpec -> T.Text
gvTransition (TSpec src dst maybeEvent gs rs) =
  T.pack $ src ++ " -> " ++ dst ++ " [ label = \"" ++ transition_text ++ "\" ]"
  where
    transition_text = trigger_name ++ "/ " ++ guard_text ++ reaction_text
    trigger_name = maybe "" (++ " ") maybeEvent
    guard_text = "" -- TODO once guards are supported
    reaction_text = foldr (\r acc -> (show r) ++ " " ++ acc) "" rs


mkGvContext rsm = makeContextText contextLookup
  where
    contextLookup key = (M.!) contextMap key
    contextMap = M.fromList [("FSM_NAME", toGVal fsm_name)
                            ,("FSM_GV_TRANSITIONS",toGVal transitions)]
    fsm_name = T.pack $ rsmName rsm
    transitions = map gvTransition (rsmTransitions rsm)


renderGvSimple :: RawStateMachine -> FilePath -> IO ()
renderGvSimple rsm outFile = do
  gvTempl <- parseGingerFile defaultTemplateLoader gvTemplateSimple
  case gvTempl of
    Left err -> do
      putStrLn "Failed to load the target!"
      exitWith (ExitFailure 2)
    Right gvTemplate -> do
      let
        gvContext  = mkGvContext rsm
      renderTemplate gvContext gvTemplate outFile
