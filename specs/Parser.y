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
    INTERNAL     { T _ (InternalT    )   }
    ID           { T _ (IdT $$       )   }
    NUM          { T _ (NumT $$      )   }
    VSEMI        { T _ VirtualSemiColonT }

%%

sisyphus : startuml transitions enduml { $2 }

startuml : STARTUML    {()}
         | {- empty -} {()}

enduml : ENDUML      {()}
       | {- empty -} {()}

semi : ';'   {()}
     | VSEMI {()}

transitions : transition             { [$1] }
            | transitions transition { $2 : $1}

transition : ID ARROW ID tbody semi        { mkTrans STKExternal $1 $3 $4    }
           | STARSTATE ARROW ID tbody semi { mkTrans STKExternal "[*]" $3 $4 }
           | ID ARROW STARSTATE tbody semi { mkTrans STKExternal $1 "[*]" $4 }
           | ID ':' INTERNAL tbody2 semi   { mkTrans STKInternal $1 $1 $4    }

tbody : {- EMPTY -} { ("",Nothing,[]) }
      | ':' tbody2  { $2 }

tbody2 : ID maybe_guard              { ($1,$2,[])}
       | ID maybe_guard '/' actions  { ($1,$2,$4)}

maybe_guard : {- EMPTY -} { Nothing }

actions :: { [SisEffect] }
actions : {- EMPTY -}    { [] }
        | actions action { $2 : $1 }

action : '@' ID { SE SEKAction $2 }
       | '^' ID { SE SEKEvent $2 }

{
data RdrEffect a = EAction a
                 | EEvent a
                 deriving (Show)

data RdrTrans = TExt String String (String,(Maybe String),[RdrEffect String])
              | TInit String (String,(Maybe String),[RdrEffect String])
              | TFinal String (String,(Maybe String),[RdrEffect String])
              | TInt String (String,(Maybe String),[RdrEffect String])
              deriving (Show)

mkTrans kind src dst (trig,guard,effs) = ST kind [trig] guard effs src dst

data RdrStmt = TransStmt [String] (SisTransition String)
             | BehaviorStmt SisBehavior
             | StateStmt (SisState String)
             | RegionStmt (SisRegion String)

happyError :: P a
happyError = failP "parse error"
}