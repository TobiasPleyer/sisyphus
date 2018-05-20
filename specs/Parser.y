{
module Sisyphus.Parser (parse) where

import Sisyphus.Types
import Sisyphus.Lexer
}

%tokentype { Token }

%name parse
%error { parseError }

%token
    '.'          { SpecialT '.'   }
    ':'          { SpecialT ':'   }
    ';'          { SpecialT ';'   }
    ','          { SpecialT ','   }
    '|'          { SpecialT '|'   }
    '{'          { SpecialT '{'   }
    '}'          { SpecialT '}'   }
    '('          { SpecialT '('   }
    ')'          { SpecialT ')'   }
    '['          { SpecialT '['   }
    ']'          { SpecialT ']'   }
    '^'          { SpecialT '^'   }
    '/'          { SpecialT '/'   }
    '@'          { SpecialT '@'   }
    ACTIONS      { ActionsT       }
    ARROW        { ArrowT         }
    EVENTS       { EventsT        }
    STATES       { StatesT        }
    TRANSITIONS  { TransitionsT   }
    entryID      { IdT "entry"    }
    exitID       { IdT "exit"     }
    internalID   { IdT "internal" }
    ID           { IdT $$         }

%%

sisyphus : events actions states transitions { RSM $1 $2 $3 $4 }

events : EVENTS event_specifiers { reverse $2 }
event_specifiers : event_specifier { [$1] }
                 | event_specifiers event_specifier { $2 : $1 }
event_specifier : ID ';' { $1 }

actions : ACTIONS action_specifiers { reverse $2 }
action_specifiers : action_specifier { [$1] }
                  | action_specifiers action_specifier { $2 : $1 }
action_specifier : ID ';' { $1 }

states : STATES state_specifiers { reverse $2 }
state_specifiers : state_specifier { [$1] }
                 | state_specifiers state_specifier { $2 : $1 }
state_specifier : ID state_attribute_list { mkState $1 $2 }

state_attribute_list : ';'                      { [] }
                     | '{' state_attributes '}' { $2 }
state_attributes : {- empty -}                          { [] }
                 | state_attribute                      { [$1] }
                 | state_attributes ',' state_attribute { $3 : $1 }
state_attribute : entryID ':' reactions                 { (ReactEntry,$3) }
                | exitID ':' reactions                  { (ReactExit,$3) }
                | internalID ':' ID '/' reactions       { (ReactInternal $3,$5)}

reactions : reaction { [$1] }
          | reactions reaction { $2 : $1 }
reaction : '@' ID { ActionCall $2 }
         | '^' ID { EventEmit $2 }

transitions : TRANSITIONS transition_specifiers { reverse $2 }
transition_specifiers : transition_specifier { [$1] }
                      | transition_specifiers transition_specifier { $2 : $1 }
transition_specifier : bar_definition                  { mkTransition (Nothing,$1) }
                     | arrow_definition bar_definition { mkTransition (Just $1,$2) }

arrow_definition : ARROW ID    { (Nothing,$2) }
                 | ID ARROW ID { (Just $1,$3) }

bar_definition : '|' ID ';' { $2 }

{

mkState name attrs =
  let
    entries = filter (isEntry . fst) attrs
    exits = filter (isExit . fst) attrs
    internals = filter (isInternal . fst) attrs
    allEntries = map ((RSpec Nothing []) . snd) entries
    allExits = map ((RSpec Nothing []) . snd) exits
    allInternals = map mkInternal internals
  in
    State name allEntries allExits allInternals

mkInternal (ReactInternal trigger, reactions) = RSpec (Just trigger) [] reactions
mkInternal _ = error "Not supported"

mkTransition (Nothing,trigger) = TSpec "" "" (Just trigger) [] []
mkTransition (Just (Nothing,dst),trigger) = TSpec "" dst (Just trigger) [] []
mkTransition (Just (Just src,dst),trigger) = TSpec src dst (Just trigger) [] []

parseError :: [Token] -> a
parseError tks = error $ show $ head tks

}
