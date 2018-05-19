{
module Main (main) where
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
  \% "events"                { \s -> EventsT }
  \% "actions"               { \s -> ActionsT }
  \% "states"                { \s -> StatesT }
  \% "transitions"           { \s -> TransitionsT }
  @id                        { \s -> ID s }

{

data Token
  = SpecialT Char
  | ArrowT
  | EventsT
  | ActionsT
  | StatesT
  | TransitionsT
  | ID String
  | EOFT
  deriving Show

main = do
  s <- getContents
  print (alexScanTokens s)
}
