{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Exception (bracket)
import Control.Monad (forM_)
import Data.Either (fromRight)
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Text.Ginger
       (makeContextText, Template, toGVal, runGinger, parseGingerFile, VarName)
import Text.Ginger.GVal (ToGVal, GVal)
import System.Exit (exitFailure)
import System.IO (IOMode(..), openFile, hClose, hGetContents)
import System.IO.Error (tryIOError)
import Sisyphus.Lexer (tokenize)
import Sisyphus.Parser (parse)
import Sisyphus.Types (RawStateMachine(..), TransitionSpec(..))
import qualified Sisyphus.Language.C.Output as C


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


loadTemplateMaybe :: FilePath -> IO (Maybe String)
loadTemplateMaybe fn =
  tryIOError (loadFile tfn) >>= \e ->
    case e of
      Right contents -> return (Just contents)
      Left _ -> return Nothing
  where
    tfn = "templates/" ++ fn

    loadFile :: FilePath -> IO String
    loadFile f = openFile f ReadMode >>= hGetContents


exportGraphviz :: RawStateMachine -> FilePath -> IO ()
exportGraphviz rsm fp =
  bracket
      (openFile fp WriteMode)
      (hClose)
      $ \fh -> do
        gvTempl <- parseGingerFile loadTemplateMaybe "Graphviz/fsm.gv.tmpl"
        case gvTempl of
          Left err -> print err >> exitFailure
          Right gvTemplate -> do
            let
              gvContext  = mkGvContext rsm
            TIO.hPutStrLn fh (runGinger gvContext gvTemplate)


main :: IO ()
main = do
  sgf <- getContents
  let rsm = parse $ tokenize sgf
  exportGraphviz rsm "test.gv"
  C.outputSimple rsm
