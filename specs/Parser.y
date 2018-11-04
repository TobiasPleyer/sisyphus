{
{-# OPTIONS_GHC -w #-}
module Sisyphus.Parser (parse, P) where

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

sisyphus : startuml decls enduml { $2 }

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
    : decls regions1  { $1 : $2 }

regions1 :: { [[RdrDecl]] }
    : regions1 region1  { $2 : $1 }
    | region1           { [$1]}
    | {- empty -}       { [] }

region1 :: { [RdrDecl] }
    : REGION decls  { $2 }

transition :: { RdrDecl }
    : ID ARROW ID tbody        semi { mkTransDecl STKExternal $1 $3 $4    }
    | STARSTATE ARROW ID tbody semi { mkTransDecl STKExternal "[*]" $3 $4 }
    | ID ARROW STARSTATE tbody semi { mkTransDecl STKExternal $1 "[*]" $4 }
    | ID ':' INTERNAL tbody2   semi { mkTransDecl STKInternal $1 $1 $4    }

behavior :: { RdrDecl }
    : ID ':' ENTRY actions semi { BehaviorDecl $1 (SBEntry $4) }
    | ID ':' EXIT actions  semi { BehaviorDecl $1 (SBExit $4) }
    | ID ':' DOACTIVITY ID semi { BehaviorDecl $1 (SBDoActivity [SE SEKDoCall $4]) }

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

data RdrDecl = StateDecl String [[RdrDecl]]
             | BehaviorDecl String SisBehavior
             | TransDecl (SisTransition String)
             deriving (Show)

mkTransDecl kind src dst (trig,guard,effs) = TransDecl $ ST kind [trig] guard effs src dst

isStateDecl :: RdrDecl -> Bool
isStateDecl (StateDecl _ _) = True
isStateDecl _ = False

isBehaviorDecl :: RdrDecl -> Bool
isBehaviorDecl (BehaviorDecl _ _) = True
isBehaviorDecl _ = False

isTransDecl :: RdrDecl -> Bool
isTransDecl (TransDecl _) = True
isTransDecl _ = False

happyError :: P a
happyError = failP "parse error"

}