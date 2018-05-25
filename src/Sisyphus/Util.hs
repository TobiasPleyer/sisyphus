module Sisyphus.Util where


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