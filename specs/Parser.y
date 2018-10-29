{
{-# OPTIONS_GHC -w #-}
module Sisyphus.Parser (parse, P) where

import Control.Monad (forM_, when)
import Data.Char

import Sisyphus.Lexer
}

%tokentype { Token }

%name parse

%monad { P } { (>>=) } { return }
%lexer { lexer } { T _ EOFT }

%token
    ';'          { T _ (SpecialT ';' ) }
    ','          { T _ (SpecialT ',' ) }
    '|'          { T _ (SpecialT '|' ) }
    '{'          { T _ (SpecialT '{' ) }
    '}'          { T _ (SpecialT '}' ) }
    '['          { T _ (SpecialT '[' ) }
    ']'          { T _ (SpecialT ']' ) }
    '/'          { T _ (SpecialT '/' ) }
    '^'          { T _ (SpecialT '^' ) }
    '@'          { T _ (SpecialT '@' ) }
    '!'          { T _ (SpecialT '!' ) }
    '='          { T _ (SpecialT '=' ) }
    '<'          { T _ (SpecialT '<' ) }
    '>'          { T _ (SpecialT '>' ) }
    ARROW        { T _ (ArrowT       ) }
    STATE        { T _ (StateT       ) }
    ENTRY        { T _ (EntryT       ) }
    EXIT         { T _ (ExitT        ) }
    INTERNAL     { T _ (InternalT    ) }
    ID           { T _ (IdT $$       ) }
    NUM          { T _ (NumT $$      ) }

%%

sisyphus : STATE ID { $2 }

{
happyError :: P a
happyError = failP "parse error"
}
