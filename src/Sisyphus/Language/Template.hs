{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module Sisyphus.Language.Template

where


import Control.Exception (bracket)
import qualified Data.Map.Strict as M
import qualified Data.Array as A
import Data.Maybe (fromJust, isJust)
import Data.Text (unpack)
import Data.Text.IO (hPutStrLn)
import System.Exit (exitFailure)
import System.IO (IOMode(..), openFile, hClose, hGetContents)
import System.IO.Error (tryIOError)
import System.FilePath
import Text.Ginger
import Sisyphus.SisSyn
import Sisyphus.Util


type TemplateLoader = FilePath -> IO (Maybe String)


defaultTemplateLoader :: FilePath -> FilePath -> IO (Maybe String)
defaultTemplateLoader templateRoot fn =
  tryIOError (loadFile tfn) >>= \e ->
    case e of
      Right contents -> return (Just contents)
      Left _ -> return Nothing
  where
    tfn = templateRoot </> fn

    loadFile :: FilePath -> IO String
    loadFile f = openFile f ReadMode >>= hGetContents


renderTemplate context template outfile =
  bracket
    (openFile outfile WriteMode)
    (hClose)
    (\fh -> hPutStrLn fh (runGinger context template))


printParseError err = do
  maybe (return ()) print (peSourcePosition err)
  putStrLn (peErrorMessage err)


renderFromFile templateLoader context outDir tmplFile targetFile = do
  let outFile = outDir </> targetFile
  parsedTmpl <- parseGingerFile templateLoader tmplFile
  case parsedTmpl of
    Left err -> do
      printParseError err
    Right tmpl -> do
      renderTemplate context tmpl outFile
