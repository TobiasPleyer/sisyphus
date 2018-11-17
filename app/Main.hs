{-# LANGUAGE RecordWildCards, DeriveDataTypeable, TupleSections #-}

module Main where

import Control.Monad (forM_, when)
import System.Console.CmdArgs
import System.FilePath
import System.Exit (exitFailure, exitSuccess, exitWith, ExitCode(..))
import System.IO (stderr, hPutStr)
import Text.Show.Pretty (pPrint)

import Sisyphus.Types
import Sisyphus.Lexer
import Sisyphus.Parser
import Sisyphus.Targets (supportedTargets, renderTarget)
import Sisyphus.Compile2


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
  } &= program "fsmg" &= summary "Sisyphus - Finite state machine generator v0.2.0.1"


main :: IO ()
main = do
  let debug = False
  opts <- cmdArgsRun options
  let file = head $ files opts
  str <- readFile file
  tkns <- case runP str tokenize of
    Left (Just (AlexPn _ line col),err) ->
            die (file ++ ":" ++ show line ++ ":" ++ show col
                             ++ ": " ++ err ++ "\n")
    Left (Nothing, err) ->
            die (file ++ ": " ++ err ++ "\n")

    Right ts -> return ts
  when debug $ do
    putStrLn "Tokens"
    pPrint tkns
    putStrLn "------"
  (pState,decls) <- case runP str parse of
    Left (Just (AlexPn _ line col),err) ->
            die (file ++ ":" ++ show line ++ ":" ++ show col
                             ++ ": " ++ err ++ "\n")
    Left (Nothing, err) ->
            die (file ++ ": " ++ err ++ "\n")

    Right (pState,decls) -> return (pState,decls)
  when debug $ do
    putStrLn "Declarations"
    pPrint decls
    putStrLn "------"
  let stage1 = runDecls decls
  when debug $ do
    putStrLn "State Machine"
    pPrint stage1
    putStrLn "------"
  when (and [ not $ null $ gsWarnings stage1
            , not $ no_warnings opts
            , not $ warn_is_error opts]) $ do
    putStrLn "== Warnings =="
    forM_ (gsWarnings stage1) putStrLn
  -- This is the final summary after the command line flags have been evaluated
  let summary = if (warn_is_error opts)
                 then
                   let errors' = (gsWarnings stage1) ++ (gsErrors stage1)
                   in stage1{gsErrors=errors'}
                 else stage1
  when (not $ null $ gsErrors summary) $ do
    putStrLn "The following errors prevent further processing:"
    putStrLn "== Errors =="
    forM_ (gsErrors summary) putStrLn
    exitFailure
  print "Done"
  exitSuccess
