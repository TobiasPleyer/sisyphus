{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module Sisyphus.Language.Template

where


import Control.Exception (bracket)
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust, isJust)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Exit (exitFailure)
import System.IO (IOMode(..), openFile, hClose, hGetContents)
import System.IO.Error (tryIOError)
import System.FilePath
import Text.Ginger
import Sisyphus.Types


defaultTemplateLoader :: FilePath -> IO (Maybe String)
defaultTemplateLoader fn =
  tryIOError (loadFile tfn) >>= \e ->
    case e of
      Right contents -> return (Just contents)
      Left _ -> return Nothing
  where
    tfn = "templates/" ++ fn

    loadFile :: FilePath -> IO String
    loadFile f = openFile f ReadMode >>= hGetContents


mkContext sm@SM{..} = makeContextText contextLookup
  where
    contextLookup = (M.!) contextMap
    contextMap = M.fromList [("FSM_NAME", gVal_FSM_NAME)
                            ,("FSM_EVENTS", gVal_FSM_EVENTS)
                            ,("FSM_STATES", gVal_FSM_STATES)
                            ,("FSM_ACTIONS", gVal_FSM_ACTIONS)
                            ,("FSM_START_STATE", gVal_FSM_START_STATE)
                            ,("FSM_STATE_MACHINE", toGVal sm)
                            ,("triggers", fromFunction triggers)
                            ]
    gVal_FSM_NAME    = toGVal $ smName
    gVal_FSM_EVENTS  = toGVal $ smEvents
    gVal_FSM_STATES  = toGVal $ M.keys smStates
    gVal_FSM_ACTIONS = toGVal $ smActions
    gVal_FSM_START_STATE = toGVal $ smStartState
    triggers args =
      let
        event = T.unpack (asText (snd (args !! 0)))
        state = T.unpack (asText (snd (args !! 1)))
      in return
         . toGVal
         . not
         . null
         . filter (==event)
         . map fromJust
         . filter isJust
         . map tspecTrigger
         $ stOutgoingTransitions (smStates M.! state)


renderTemplate context template outfile =
  bracket
    (openFile outfile WriteMode)
    (hClose)
    (\fh -> TIO.hPutStrLn fh (runGinger context template))


printParseError err = do
  maybe (return ()) print (peSourcePosition err)
  putStrLn (peErrorMessage err)


renderFromFile context outDir tmplFile targetFile = do
  let outFile = outDir </> targetFile
  parsedTmpl <- parseGingerFile defaultTemplateLoader tmplFile
  case parsedTmpl of
    Left err -> do
      printParseError err
    Right tmpl -> do
      renderTemplate context tmpl outFile
