{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Sisyphus.Language.Template
( defaultTemplateLoader
, renderTemplate
)where


import Control.Exception (bracket)
import qualified Data.Text.IO as TIO
import System.Exit (exitFailure)
import System.IO (IOMode(..), openFile, hClose, hGetContents)
import System.IO.Error (tryIOError)
import Text.Ginger
       (makeContextText, Template, toGVal, runGinger, parseGingerFile, VarName)


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


renderTemplate context template outfile =
  bracket
    (openFile outfile WriteMode)
    (hClose)
    (\fh -> TIO.hPutStrLn fh (runGinger context template))
