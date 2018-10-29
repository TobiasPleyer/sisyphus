{
module Sisyphus.Lexer

where

import Data.Char

import Control.Applicative ( Applicative(..) )
import Control.Monad ( liftM, ap )
import Data.Word (Word8)
import qualified Sisyphus.Util as Util
import Sisyphus.SisSyn

}

$digit    = 0-9
$octal    = 0-7
$lower    = a-z
$upper    = A-Z
$alpha    = [$upper $lower]
$alphanum = [$alpha $digit]
$idchar   = [$alphanum \_]
$nl       = [\n\r\f]
$white_no_nl = $white # $nl

$special  = [\^\.\:\;\,\$\@\|\*\+\?\~\-\{\}\(\)\[\]\/\<\>\=\!]

@id         = $alpha $idchar*
@num        = $digit+
@comment    = "#".*
@arrow      = "-"+">"
@regionLine = "-"+"-"

tokens :-

$white_no_nl+              { skip      }
$nl                        { vSemi     }
@comment                   { skip      }
@arrow                     { arr       }
@regionLine                { region    }
"[*]"                      { star      }
"@startuml"                { startUml  }
"@enduml"                  { endUml    }
$special                   { special   }
"state"                    { state     }
"entry" \:                 { entry     }
"exit" \:                  { exit      }
"internal" \:              { internal  }
@id                        { ident     }
@num                       { number    }

{

-- -----------------------------------------------------------------------------
-- Token type

data Token = T AlexPosn Tkn
  deriving Show

tokPosn (T p _) = p

data Tkn
  = NoneT
  | SpecialT Char
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
  | VirtualSemiColonT
  | EOFT
  deriving Show

-- -----------------------------------------------------------------------------
-- Token functions

type Action = (AlexPosn,Char,String) -> Int -> P Token

special, startUml, endUml, arr, star, region, state :: Action
entry, exit, internal, ident, number, vSemi :: Action

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
vSemi     (p,_,_)   _  = do pTkn <- getPrevToken
                            case pTkn of
                              SpecialT _ -> lexToken
                              RegionLineT -> lexToken
                              StartUmlT -> lexToken
                              EndUmlT -> lexToken
                              _ -> return $ T p VirtualSemiColonT

skip :: Action
skip _ _ = lexToken

-- -----------------------------------------------------------------------------
-- The input type

type Byte = Word8

type AlexInput = (AlexPosn,     -- current position,
                  Char,         -- previous char
                  [Byte],
                  String)       -- current input string

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar (_,c,_,_) = c


alexGetChar :: AlexInput -> Maybe (Char,AlexInput)
alexGetChar (_,_,[],[]) = Nothing
alexGetChar (p,_,[],(c:s))  = let p' = alexMove p c in p' `seq`
                                Just (c, (p', c, [], s))
alexGetChar (_, _ ,_ : _, _) = undefined -- hide compiler warning

alexGetByte :: AlexInput -> Maybe (Byte,AlexInput)
alexGetByte (p,c,(b:bs),s) = Just (b,(p,c,bs,s))
alexGetByte (_,_,[],[]) = Nothing
alexGetByte (p,_,[],(c:s))  = let p' = alexMove p c
                                  (b:bs) = Util.encode c
                              in p' `seq`  Just (b, (p', c, bs, s))

-- -----------------------------------------------------------------------------
-- Token positions

-- `Posn' records the location of a token in the input text.  It has three
-- fields: the address (number of chacaters preceding the token), line number
-- and column of a token within the file. `start_pos' gives the position of the
-- start of the file and `eof_pos' a standard encoding for the end of file.
-- `move_pos' calculates the new position after traversing a given character,
-- assuming the usual eight character tab stops.

data AlexPosn = AlexPn !Int !Int !Int
        deriving (Eq,Show)

alexStartPos :: AlexPosn
alexStartPos = AlexPn 0 1 1

alexMove :: AlexPosn -> Char -> AlexPosn
alexMove (AlexPn a l c) '\t' = AlexPn (a+1)  l    (((c+7) `div` 8)*8+1)
alexMove (AlexPn a l _) '\n' = AlexPn (a+1) (l+1)  1
alexMove (AlexPn a l c) _    = AlexPn (a+1)  l    (c+1)

-- -----------------------------------------------------------------------------
-- Alex lexing/parsing monad

type ParseError = (Maybe AlexPosn, String)
type StartCode = Int

data PState = PState { startcode    :: Int
                     , input        :: AlexInput
                     , prevToken    :: Tkn
                     , scope        :: [String]
                     , transitions  :: [SisTransition String]
                     , warnings     :: [String]
                     , errors       :: [String]
                     }

newtype P a = P { unP :: PState -> Either ParseError (PState,a) }

instance Functor P where
  fmap = liftM

instance Applicative P where
  pure a = P $ \env -> Right (env,a)
  (<*>) = ap

instance Monad P where
 (P m) >>= k = P $ \env -> case m env of
                        Left err -> Left err
                        Right (env',ok) -> unP (k ok) env'
 return = pure

runP :: String -> P a -> Either ParseError a
runP str (P p)
  = case p initial_state of
        Left err -> Left err
        Right (_,a) -> Right a
 where initial_state = PState{ startcode = 0
                             , input = (alexStartPos,'\n',[],str)
                             , prevToken = NoneT
                             , scope = []
                             , transitions = []
                             , warnings = []
                             , errors = []
                             }

failP :: String -> P a
failP str = P $ \PState{ input = (p,_,_,_) } -> Left (Just p,str)

getPrevToken :: P Tkn
getPrevToken = P $ \st -> Right (st, prevToken st)

setPrevToken :: Tkn -> P ()
setPrevToken t = P $ \st -> Right (st{ prevToken = t }, ())

getScope :: P [String]
getScope = P $ \st -> Right (st, scope st)

setScope :: [String] -> P ()
setScope sc = P $ \st -> Right (st{ scope = sc }, ())

pushScope :: String -> P ()
pushScope s = P $ \st -> Right (st{ scope = s:(scope st) }, ())

popScope :: P String
popScope = do
  sc <- getScope
  case sc of
    [] -> failP "Popping from empty scope stack!"
    s:sc' -> do
      setScope sc'
      return s

setStartCode :: StartCode -> P ()
setStartCode sc = P $ \s -> Right (s{ startcode = sc }, ())

getStartCode :: P StartCode
getStartCode = P $ \s -> Right (s, startcode s)

getInput :: P AlexInput
getInput = P $ \s -> Right (s, input s)

setInput :: AlexInput -> P ()
setInput inp = P $ \s -> Right (s{ input = inp }, ())

getTransitions :: P [SisTransition String]
getTransitions = P $ \st -> Right (st, transitions st)

setTransitions :: [SisTransition String] -> P ()
setTransitions ts = P $ \st -> Right (st{ transitions = ts }, ())

addTransition :: (SisTransition String) -> P ()
addTransition t = P $ \st -> Right (st{ transitions = t:(transitions st) }, ())

getWarnings :: P [String]
getWarnings = P $ \st -> Right (st, warnings st)

setWarnings :: [String] -> P ()
setWarnings ws = P $ \st -> Right (st{ warnings = ws }, ())

addWarning :: String -> P ()
addWarning w = P $ \st -> Right (st{ warnings = w:(warnings st) }, ())

getErrors :: P [String]
getErrors = P $ \st -> Right (st, errors st)

setErrors :: [String] -> P ()
setErrors es = P $ \st -> Right (st{ errors = es }, ())

addError :: String -> P ()
addError e = P $ \st -> Right (st{ errors = e:(errors st) }, ())


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
    AlexToken inp1 len a -> do
      setInput inp1
      t@(T p tkn) <- a (p,c,s) len
      setPrevToken tkn
      return t

tokenize :: P [Tkn]
tokenize = go []
  where go ts = do (T _ t) <- lexToken
                   case t of
                     EOFT -> return $ reverse ts
                     _ -> go (t:ts)

begin :: StartCode -> Action
begin code _ai _l = do setStartCode code; lexToken

andBegin :: Action -> StartCode -> Action
andBegin act sc inp len = setStartCode sc >> act inp len
}
