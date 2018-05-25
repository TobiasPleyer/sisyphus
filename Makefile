HAPPY = happy
HAPPY_OPTS = 

ALEX = alex
ALEX_OPTS = 

all:
	alex -o src/Sisyphus/Lexer.hs specs/Lexer.x
	happy -o src/Sisyphus/Parser.hs specs/Parser.y
	stack build
