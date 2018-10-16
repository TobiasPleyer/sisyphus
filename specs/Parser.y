{
{-# OPTIONS_GHC -w #-}
module Sisyphus.Parser (parse, P) where

import Data.Char
import qualified Data.Map.Strict as M
import Control.Arrow ((&&&))

import Sisyphus.ParseMonad hiding ( StartCode )
import Sisyphus.Types
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
    NAME         { T _ (NameT        ) }
    EVENTS       { T _ (EventsT      ) }
    ACTIONS      { T _ (ActionsT     ) }
    STATES       { T _ (StatesT      ) }
    TRANSITIONS  { T _ (TransitionsT ) }
    ENTRY        { T _ (EntryT       ) }
    EXIT         { T _ (ExitT        ) }
    INTERNAL     { T _ (InternalT    ) }
    ID           { T _ (IdT $$       ) }
    NUM          { T _ (NumT $$      ) }

%%

sisyphus : name events actions states transitions { SM $1 $2 $3 $4 $5 }

name : NAME ID     { $2 }
     | {- empty -} { "FSM" }

events : {- empty -}                                { [] }
       | EVENTS event_specifiers                    { reverse $2 }
event_specifiers : event_specifier                  { [$1] }
                 | event_specifiers event_specifier { $2 : $1 }
event_specifier : ID ';'                            { $1 }

actions : {- empty -}                                  { [] }
        | ACTIONS action_specifiers                    { reverse $2 }
action_specifiers : action_specifier                   { [$1] }
                  | action_specifiers action_specifier { $2 : $1 }
action_specifier : ID ';'                              { $1 }

states : {- empty -}                                { M.empty }
       | STATES state_specifiers                    { M.fromList $ map (stName &&& id) $2 }
state_specifiers : state_specifier                  { [$1] }
                 | state_specifiers state_specifier { $2 : $1 }
state_specifier : ID state_attribute_list           { mkState $1 $2 }

state_attribute_list : ';'                              { [] }
                     | '{' state_attributes '}'         { $2 }
state_attributes : {- empty -}                          { [] }
                 | state_attribute                      { [$1] }
                 | state_attributes ',' state_attribute { $3 : $1 }
state_attribute : ENTRY reactions                       { ReactEntry $2 }
                | EXIT reactions                        { ReactExit $2 }
                | INTERNAL ID guards '/' reactions      { ReactInternal $2 $3 $5 }

reactions : {- empty -}        { [] }
          | reactions reaction { $2 : $1 }
reaction : '@' ID              { ActionCall $2 }
         | '^' ID              { EventEmit $2 }

transitions : TRANSITIONS bar_arrow_definitions                    { expandTransitions $ reverse $2 }
bar_arrow_definitions : bar_arrow_definition                       { [$1] }
                      | bar_arrow_definitions bar_arrow_definition { $2 : $1 }
bar_arrow_definition : bar_definition                              { mkTransition (Nothing,$1) }
                     | arrow_definition bar_definition             { mkTransition (Just $1,$2) }

arrow_definition : ARROW ID    { (Nothing,$2) }
                 | ID ARROW ID { (Just $1,$3) }

bar_definition : '|' ID ';'                       { ($2,[],[]) }
               | '|' ID guards '/' reactions ';'  { ($2,$3,$5) }

guards : {- empty -}   { [] }
       | guards guard  { $2 : $1 }
guard : '[' guard_test ']'  { $2 }

guard_test : '!' ID          { NotG $2 }
           | ID '=' '=' ID   { BinOpG OpEQ  (V $1) (V $4) }
           | ID '!' '=' ID   { BinOpG OpNEQ (V $1) (V $4) }
           | ID '<' ID       { BinOpG OpLT  (V $1) (V $3) }
           | ID '<' '=' ID   { BinOpG OpLE  (V $1) (V $4) }
           | ID '>' ID       { BinOpG OpGT  (V $1) (V $3) }
           | ID '>' '=' ID   { BinOpG OpGE  (V $1) (V $4) }
           | ID '=' '=' NUM  { BinOpG OpEQ  (V $1) (C $4) }
           | ID '!' '=' NUM  { BinOpG OpNEQ (V $1) (C $4) }
           | ID '<' NUM      { BinOpG OpLT  (V $1) (C $3) }
           | ID '<' '=' NUM  { BinOpG OpLE  (V $1) (C $4) }
           | ID '>' NUM      { BinOpG OpGT  (V $1) (C $3) }
           | ID '>' '=' NUM  { BinOpG OpGE  (V $1) (C $4) }
           | ID              { G $1 }

{
happyError :: P a
happyError = failP "parse error"

mkState name attrs =
  let
    entries = filter isEntry attrs
    exits = filter isExit attrs
    internals = filter isInternal attrs
    allEntries = map (\(ReactEntry rs) -> RSpec Nothing [] rs) entries
    allExits = map (\(ReactExit rs) -> RSpec Nothing [] rs) exits
    allInternals = map (\(ReactInternal trig gs rs) -> RSpec (Just trig) gs rs) internals
  in
    State name allEntries allExits allInternals [] []

mkTransition (Nothing,(t,gs,rs)) = (Nothing,Nothing,Just t,gs,rs)
mkTransition (Just (Nothing,dst),(t,gs,rs)) = (Nothing,Just dst,Just t,gs,rs)
mkTransition (Just (Just src,dst),(t,gs,rs)) = (Just src,Just dst,Just t,gs,rs)

{-
In grammar files it is possible to leave out source and destination if they are
the same for the next transition definition. This function expands them so that
each of them can exist as stand alone transitions, without the need of the
context of the previous definitions.
-}
expandTransitions :: [(Maybe String, Maybe String, Maybe Event, [Guard], [Reaction])] -> [TransitionSpec]
expandTransitions ts = go Nothing Nothing ts
  where
    go _ _ [] = []
    go _ _ ts@(t@(Just src,Just dst,Just trigg,gs,rs):ts') = (TSpec src dst (Just trigg) gs rs) : go (Just src) (Just dst) ts'
    go (Just src) _ ts@(t@(Nothing,Just dst,Just trigg,gs,rs):ts') = (TSpec src dst (Just trigg) gs rs) : go (Just src) (Just dst) ts'
    go (Just src) (Just dst) ts@(t@(Nothing,Nothing,Just trigg,gs,rs):ts') = (TSpec src dst (Just trigg) gs rs) : go (Just src) (Just dst) ts'
    go _ _ _ = error "Invalid transition definition!"


parseError :: [Token] -> a
parseError tks = error $ "Unexpected token: " ++ (show $ head tks)

}
