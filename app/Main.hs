{-# LANGUAGE RecordWildCards, DeriveDataTypeable, TupleSections #-}

module Main where

import Control.Monad (forM_, when)
import System.Console.CmdArgs
import System.FilePath
import System.Exit (exitFailure, exitSuccess)

import Sisyphus.Lexer (tokenize)
import Sisyphus.Parser (parse)
import Sisyphus.Types
import Sisyphus.Compile
import Sisyphus.Targets (supportedTargets, tryRenderTarget)


data Options = Options
  { no_warnings :: Bool
  , warn_is_error :: Bool
  , graphviz :: Bool
  , print_statemachine :: Bool
  , outputdir :: FilePath
  , input_format :: String
  , target :: String
  , files :: [FilePath]
  } deriving (Data,Typeable,Show)


options :: Mode (CmdArgs Options)
options = cmdArgsMode $ Options
  { no_warnings        = False            &= name "W" &= help "Don't print warnings"
  , warn_is_error      = False            &= name "E" &= help "Warnings are treated as errors"
  , graphviz           = False            &= name "G" &= help "Generate a Graphviz file of the FSM"
  , print_statemachine = False            &= name "p" &= help "Print the parsed state machine"
  , outputdir     = "."   &= typDir       &= name "d" &= help "Output will go in this directory"
  , input_format  = "sgf" &= typ "INPUT"  &= name "i" &= help "The input file format"
  , target        = ""    &= typ "TARGET" &= name "t" &= help "The target language of the generated FSM"
  , files         = []    &= typ "PATH"   &= args
  } &= program "fsmg" &= summary "Sisyphus - Finite state machine generator v0.1beta"


main :: IO ()
main = do
  opts <- cmdArgsRun options
  sgf <- readFile $ head $ files opts
  let sm = parse $ tokenize sgf
  let smry = runChecks sm
  when (and [ not $ null $ warnings smry
            , not $ no_warnings opts
            , not $ warn_is_error opts]) $ do
    putStrLn "== Warnings =="
    forM_ (warnings smry) putStrLn
  -- This is the final summary after the command line flags have been evaluated
  let summary = if (warn_is_error opts)
                 then
                   let errors' = (warnings smry) ++ (errors smry)
                   in smry{errors=errors'}
                 else smry
  when (not $ null $ errors summary) $ do
    putStrLn "The following errors prevent further processing:"
    putStrLn "== Errors =="
    forM_ (errors summary) putStrLn
    exitFailure
  -- This is the state machine after the checks have been run
  let statemachine = stateMachine summary
  when (graphviz opts) $ do
    let gvPath = (outputdir opts) </> (smName sm) <.> "gv"
    tryRenderTarget "Graphviz_Simple" statemachine gvPath
  when (print_statemachine opts) $
    print statemachine
  print "Done"
  exitSuccess
