{
module Sisyphus.Lexer (lexer, AlexPosn(..), Token(..), Tkn(..), tokPosn) where

import Data.Char

import Sisyphus.ParseMonad

}

$digit    = 0-9
$octal    = 0-7
$lower    = a-z
$upper    = A-Z
$alpha    = [$upper $lower]
$alphanum = [$alpha $digit]
$idchar   = [$alphanum \_]

$special  = [\^\.\:\;\,\$\@\|\*\+\?\~\-\{\}\(\)\[\]\/\<\>\=\!]

@id         = $alpha $idchar*
@num        = $digit+
@comment    = "#".*
@arrow      = "-"+">"
@regionLine = "-"+"-"

tokens :-

<0> $white+                    { skip     }
<0> @comment                   { skip     }
<0> @arrow                     { arr      }
<0> @regionLine                { region   }
<0> "[*]"                      { star     }
<0> $special                   { special  }
<0> "state"                    { state    }
<0> "entry" \:                 { entry    }
<0> "exit" \:                  { exit     }
<0> "internal" \:              { internal }
<0> @id                        { ident    }
<0> @num                       { number   }

{

-- -----------------------------------------------------------------------------
-- Token type

data Token = T AlexPosn Tkn
  deriving Show

tokPosn (T p _) = p

data Tkn
  = SpecialT Char
  | StartUmlT
  | EndUmlT
  | ArrowT
  | StarStateT
  | RegionLineT
  | StateT
  | EntryT
  | ExitT
  | InternalT
  | IdT String
  | NumT String
  | EOFT
  deriving Show

-- -----------------------------------------------------------------------------
-- Token functions

type Action = (AlexPosn,Char,String) -> Int -> P Token

special, startUml, endUml, arr, star, region, state :: Action
entry, exit, internal, ident, number :: Action

special   (p,_,str) _  = return $ T p (SpecialT  (head str))
startUml  (p,_,_)   _  = return $ T p StartUmlT
endUml    (p,_,_)   _  = return $ T p EndUmlT
arr       (p,_,_)   _  = return $ T p ArrowT
star      (p,_,_)   _  = return $ T p StarStateT
region    (p,_,_)   _  = return $ T p RegionLineT
state     (p,_,_)   _  = return $ T p StateT
entry     (p,_,_)   _  = return $ T p EntryT
exit      (p,_,_)   _  = return $ T p ExitT
internal  (p,_,_)   _  = return $ T p InternalT
ident     (p,_,str) ln = return $ T p (IdT (take ln str))
number    (p,_,str) ln = return $ T p (NumT (take ln str))


lexError :: String -> P a
lexError s = do
  (_,_,_,input) <- getInput
  failP (s ++ (if (not (null input))
                  then " at " ++ show (head input)
                  else " at end of file"))

lexer :: (Token -> P a) -> P a
lexer cont = lexToken >>= cont

lexToken :: P Token
lexToken = do
  inp@(p,c,_,s) <- getInput
  sc <- getStartCode
  case alexScan inp sc of
    AlexEOF -> return (T p EOFT)
    AlexError _ -> lexError "lexical error"
    AlexSkip inp1 _ -> do
      setInput inp1
      lexToken
    AlexToken inp1 len t -> do
      setInput inp1
      t (p,c,s) len

skip :: Action
skip _ _ = lexToken

andBegin :: Action -> StartCode -> Action
andBegin act sc inp len = setStartCode sc >> act inp len
}
