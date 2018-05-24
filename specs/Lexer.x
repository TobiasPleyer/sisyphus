{
module Sisyphus.Lexer (Token(..), tokenize) where
}

%wrapper "basic"

$digit    = 0-9
$octal    = 0-7
$lower    = a-z
$upper    = A-Z
$alpha    = [$upper $lower]
$alphanum = [$alpha $digit]
$idchar   = [$alphanum \_]

$special  = [\.\:\;\,\$\@\|\*\+\?\~\-\{\}\(\)\[\]\/]

@id       = $alpha $idchar*
@comment  = "#".*

tokens :-

  $white+                    ;
  @comment                   ;
  "->"                       { \s -> ArrowT }
  $special                   { \s -> SpecialT (head s) }
  \% "name"                  { \s -> NameT }
  \% "events"                { \s -> EventsT }
  \% "actions"               { \s -> ActionsT }
  \% "states"                { \s -> StatesT }
  \% "transitions"           { \s -> TransitionsT }
  @id                        { \s -> IdT s }

{

data Token
  = SpecialT Char
  | ArrowT
  | NameT
  | EventsT
  | ActionsT
  | StatesT
  | TransitionsT
  | IdT String
  | EOFT
  deriving Show

tokenize = alexScanTokens
}
