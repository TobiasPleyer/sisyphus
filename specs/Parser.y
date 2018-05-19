{
module Sisyphus.Parser (parse) where

import Sisyphus.Types
import Sisyphus.Lexer
}

%tokentype { Token }

%name parse
%error { parseError }

%token
    '.'          { SpecialT '.' }
    ':'          { SpecialT ':' }
    ';'          { SpecialT ';' }
    ','          { SpecialT ',' }
    '|'          { SpecialT '|' }
    '{'          { SpecialT '{' }
    '}'          { SpecialT '}' }
    '('          { SpecialT '(' }
    ')'          { SpecialT ')' }
    '['          { SpecialT '[' }
    ']'          { SpecialT ']' }
    '^'          { SpecialT '^' }
    '/'          { SpecialT '/' }
    ACTIONS      { ActionsT }
    ARROW        { ArrowT }
    EVENTS       { EventsT }
    STATES       { StatesT }
    TRANSITIONS  { TransitionsT }
    ID           { IdT $$ }

%%

sisyphus : events actions { RSM $1 $2 [] [] }

events : EVENTS event_specifiers { $2 }
event_specifiers : event_specifier { [$1] }
                 | event_specifiers event_specifier { $2 : $1 }
event_specifier : ID ';' { $1 }

actions : ACTIONS action_specifiers { $2 }
action_specifiers : action_specifier { [$1] }
                  | action_specifiers action_specifier { $2 : $1 }
action_specifier : ID ';' { $1 }

-- states : STATES state_specifiers { $2 }
-- state_specifiers : state_specifier state_specifiers { $1 : $2 }
-- state_specifier : ID state_attribute_list { State $1 }
-- 
-- state_attribute_list : ';'                      { [] }
--                      | '{' state_attributes '}' { $2 }
-- state_attributes : {- empty -}                          { [] }
--                  | state_attribute                      { [$1] }
--                  | state_attributes ',' state_attribute { $3 : $1 }
-- state_attribute : ID ':' reactions                      { make_state_attribute $1 "" $3}
--                 | ID ':' ID '/' reactions               { make_state_attribute $1 $3 $5}

{

parseError :: [Token] -> a
parseError tks = error $ show $ head tks

data WhenClassifier = Entry
                    | Exit
                    | Internal
type StateAttribute = (WhenClassifier,Reaction)

}
