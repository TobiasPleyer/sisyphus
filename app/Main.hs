{-# LANGUAGE RecordWildCards, DeriveDataTypeable, TupleSections #-}

module Main where

import Control.Monad (forM_, when)
import System.Console.CmdArgs
import System.FilePath
import System.Exit (exitFailure, exitSuccess, exitWith, ExitCode(..))
import System.IO (stderr, hPutStr)

import Sisyphus.Types
import Sisyphus.ParseMonad hiding (errors, warnings)
import Sisyphus.Parser
import Sisyphus.Targets (supportedTargets, renderTarget)


bye :: String -> IO a
bye s = putStr s >> exitWith ExitSuccess

die :: String -> IO a
die s = hPutStr stderr s >> exitWith (ExitFailure 1)


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
  } &= program "fsmg" &= summary "Sisyphus - Finite state machine generator v0.1beta"


main :: IO ()
main = do
  opts <- cmdArgsRun options
  let file = head $ files opts
  sgf <- readFile file
  smry <- case runP sgf parse of
    Left (Just (AlexPn _ line col),err) ->
            die (file ++ ":" ++ show line ++ ":" ++ show col
                             ++ ": " ++ err ++ "\n")
    Left (Nothing, err) ->
            die (file ++ ": " ++ err ++ "\n")

    Right grammarSummary -> return grammarSummary
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
  forM_ (targets opts) $ renderTarget statemachine (outputdir opts)
  when (print_statemachine opts) $
    print statemachine
  print "Done"
  exitSuccess
