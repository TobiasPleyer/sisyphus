{
{-# OPTIONS_GHC -w #-}
module Sisyphus.Parser

where

import Control.Monad (forM_, when)
import Data.Char

import Sisyphus.SisSyn
import Sisyphus.Lexer
}

%tokentype { Token }

%name parse

%monad { P } { (>>=) } { return }
%lexer { lexer } { T _ EOFT }

%token
    STARTUML     { T _ StartUmlT         }
    ENDUML       { T _ EndUmlT           }
    '.'          { T _ (SpecialT '.' )   }
    ':'          { T _ (SpecialT ':' )   }
    ';'          { T _ (SpecialT ';' )   }
    ','          { T _ (SpecialT ',' )   }
    '|'          { T _ (SpecialT '|' )   }
    '{'          { T _ (SpecialT '{' )   }
    '}'          { T _ (SpecialT '}' )   }
    '['          { T _ (SpecialT '[' )   }
    ']'          { T _ (SpecialT ']' )   }
    '/'          { T _ (SpecialT '/' )   }
    '^'          { T _ (SpecialT '^' )   }
    '@'          { T _ (SpecialT '@' )   }
    '!'          { T _ (SpecialT '!' )   }
    '='          { T _ (SpecialT '=' )   }
    '<'          { T _ (SpecialT '<' )   }
    '>'          { T _ (SpecialT '>' )   }
    ARROW        { T _ (ArrowT       )   }
    STARSTATE    { T _ StarStateT        }
    REGION       { T _ RegionSepT        }
    STATE        { T _ (StateT       )   }
    ENTRY        { T _ (EntryT       )   }
    EXIT         { T _ (ExitT        )   }
    DOACTIVITY   { T _ (DoActivityT  )   }
    INTERNAL     { T _ (InternalT    )   }
    ID           { T _ (IdT $$       )   }
    NUM          { T _ (NumT $$      )   }
    VSEMI        { T _ VirtualSemiColonT }

%%

sisyphus : startuml decls enduml { reverse $2 }

startuml : STARTUML    {()}
         | {- empty -} {()}

enduml : ENDUML      {()}
       | {- empty -} {()}

semi : ';'   {()}
     | VSEMI {()}

decls :: { [RdrDecl] }
    : decls decl  { $2 : $1}
    | decl        { [$1] }
    | {- empty -} { [] }

decl :: { RdrDecl }
    : state       { $1 }
    | behavior    { $1 }
    | transition  { $1 }

state :: { RdrDecl }
    : STATE ID semi             { StateDecl $2 [] }
    | STATE ID '{' regions '}'  { StateDecl $2 $4 }

regions :: { [[RdrDecl]] }
    : decls regions1  { (reverse $1) : (reverse $2) }

regions1 :: { [[RdrDecl]] }
    : regions1 region1  { $2 : $1 }
    | region1           { [$1]}
    | {- empty -}       { [] }

region1 :: { [RdrDecl] }
    : REGION decls  { reverse $2 }

transition :: { RdrDecl }
    : rid ARROW rid tbody       semi { mkTransDecl STKExternal $1 $3 $4    }
    | STARSTATE ARROW rid tbody semi { mkTransDecl STKExternal (UnqualId "[*]") $3 $4 }
    | rid ARROW STARSTATE tbody semi { mkTransDecl STKExternal $1 (UnqualId "[*]") $4 }
    | rid ':' INTERNAL tbody2   semi { mkTransDecl STKInternal $1 $1 $4    }

behavior :: { RdrDecl }
    : rid ':' ENTRY actions semi { BehaviorDecl $1 (SBEntry $4) }
    | rid ':' EXIT actions  semi { BehaviorDecl $1 (SBExit $4) }
    | rid ':' DOACTIVITY ID semi { BehaviorDecl $1 (SBDoActivity [SE SEKDoCall $4]) }

rid :: { RdrId }
    : rid1 ID    { QualId (reverse ($2 : $1)) }
    | ID         { UnqualId $1 }

rid1 :: { [String] }
    : rid1 ID '.'  { $2 : $1 }
    | ID '.'       { [$1] }

tbody : {- EMPTY -} { ("",Nothing,[]) }
      | ':' tbody2  { $2 }

tbody2 : ID maybe_guard              { ($1,$2,[]) }
       | ID maybe_guard '/' actions  { ($1,$2,$4) }

maybe_guard : {- EMPTY -} { Nothing }

actions :: { [SisEffect] }
actions : {- EMPTY -}    { [] }
        | actions action { $2 : $1 }

action : '@' ID { SE SEKAction $2 }
       | '^' ID { SE SEKEvent $2 }

{

mkTransDecl kind src dst (trig,guard,effs) = TransDecl $ ST kind [trig] guard effs src dst

happyError :: P a
happyError = failP "parse error"

}