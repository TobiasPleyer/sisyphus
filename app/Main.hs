module Main where

import Control.Exception (bracket)
import Control.Monad (forM_)
import System.IO (IOMode(..), openFile, hClose, hPutStr, hPutStrLn)
import Sisyphus.Lexer (tokenize)
import Sisyphus.Parser (parse)
import Sisyphus.Types (RawStateMachine(..), TransitionSpec(..))
import qualified Sisyphus.Language.C.Output as C


exportGraphviz :: RawStateMachine -> FilePath -> IO ()
exportGraphviz rsm fp =
  bracket
    (openFile fp WriteMode)
    (hClose)
    $ \fh -> do
      hPutStrLn fh "digraph finite_state_machine {\n    rankdir=LR;\n    node [shape = box];"
      forM_ (rsmTransitions rsm) (hPutStrLn fh . gvTransition)
      hPutStr fh "}"
      where
        gvTransition :: TransitionSpec -> String
        gvTransition (TSpec src dst maybeEvent gs rs) = "    " ++ src ++ " -> " ++ dst ++ " [ label = \"" ++ transition_text ++ "\" ];"
          where
            transition_text = trigger_name ++ "/ " ++ guard_text ++ reaction_text
            trigger_name = maybe "" (++ " ") maybeEvent
            guard_text = "" -- TODO once guards are supported
            reaction_text = foldr (\r acc -> (show r) ++ " " ++ acc) "" rs


main :: IO ()
main = do
  sgf <- getContents
  let rsm = parse $ tokenize sgf
  exportGraphviz rsm "test.gv"
  C.outputSimple rsm
