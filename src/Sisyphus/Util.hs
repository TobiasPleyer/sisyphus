module Sisyphus.Util where


str :: String -> String -> String
str = showString

char :: Char -> String -> String
char c = (c :)

nl :: String -> String
nl = char '\n'

paren :: (String -> String) -> String -> String
paren s = char '(' . s . char ')'

brack :: (String -> String) -> String -> String
brack s = char '[' . s . char ']'

interleave_shows :: (String -> String) -> [String -> String] -> String -> String
interleave_shows _ [] = id
interleave_shows s xs = foldr1 (\a b -> a . s . b) xs

space :: String -> String
space = char ' '
