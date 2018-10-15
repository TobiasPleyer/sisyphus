module Sisyphus.Util where


import Data.Maybe
import Sisyphus.Types


str :: String -> String -> String
str = showString

char :: Char -> String -> String
char c = (c :)

nl :: String -> String
nl = char '\n'

nl2 :: String -> String
nl2 = str "\n\n"

nl3 :: String -> String
nl3 = str "\n\n\n"

enclose :: String -> String -> String -> String -> String
enclose left right middle = str left . str middle . str right

paren :: (String -> String) -> String -> String
paren s = char '(' . s . char ')'

brack :: (String -> String) -> String -> String
brack s = char '[' . s . char ']'

interleave_shows :: (String -> String) -> [String -> String] -> String -> String
interleave_shows _ [] = id
interleave_shows s xs = foldr1 (\a b -> a . s . b) xs

space :: String -> String
space = char ' '

addIngoingTransition :: TransitionSpec -> State -> State
addIngoingTransition t s =
  let
    ins = stIngoingTransitions s
    s' = s{stIngoingTransitions=(t:ins)}
  in s'

addOutgoingTransition :: TransitionSpec -> State -> State
addOutgoingTransition t s =
  let
    outs = stOutgoingTransitions s
    s' = s{stOutgoingTransitions=(t:outs)}
  in s'


isTriggeredBy :: State -> Event -> Bool
isTriggeredBy state event = not
                          . null
                          . filter (==event)
                          . map fromJust
                          . filter isJust
                          . concat
                          $ [ map tspecTrigger (stOutgoingTransitions state)
                            , map rspecTrigger (stInternalReactions state)
                            ]

