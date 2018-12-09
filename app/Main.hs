{-# LANGUAGE RecordWildCards, DeriveDataTypeable, TupleSections #-}

module Main where

import Control.Monad (forM_, when)
import qualified Data.Array as A
import System.Console.CmdArgs
import System.Exit (exitFailure, exitSuccess, exitWith, ExitCode(..))
import System.FilePath

import Sisyphus.Lexer
import Sisyphus.Parser
import Sisyphus.SisSyn
import Sisyphus.Targets (supportedTargets, renderTarget)
import Sisyphus.Compile


data Options = Options
  { no_warnings :: Bool
  , warn_is_error :: Bool
  , print_statemachine :: Bool
  , outputdir :: FilePath
  , input_format :: String
  , targets :: [String]
  , files :: [FilePath]
  } deriving (Data,Typeable,Show)


options :: Mode (CmdArgs Options)
options = cmdArgsMode $ Options
  { no_warnings        = False            &= name "W" &= help "Don't print warnings"
  , warn_is_error      = False            &= name "E" &= help "Warnings are treated as errors"
  , print_statemachine = False            &= name "p" &= help "Print the parsed state machine"
  , outputdir     = "."   &= typDir       &= name "d" &= help "Output will go in this directory"
  , input_format  = "sgf" &= typ "INPUT"  &= name "i" &= help "The input file format"
  , targets       = []    &= typ "TARGET" &= name "t" &= help "The target language of the generated FSM"
  , files         = []    &= typ "PATH"   &= args
  } &= program "fsmg" &= summary "Sisyphus - Finite state machine generator v0.2.0.1"


main :: IO ()
main = do
  opts <- cmdArgsRun options
  let file = head $ files opts
  stateMachine <- compile (warn_is_error opts) (no_warnings opts) file
  when ((snd (A.bounds (smRegionArray stateMachine))) > 1) $ do
    putStrLn "Currently Sisyphus does not support hierarchical state machines."
    exitFailure
  forM_ (targets opts) $ renderTarget stateMachine (outputdir opts)
  when (print_statemachine opts) $ print stateMachine
  exitSuccess
  print "Done"