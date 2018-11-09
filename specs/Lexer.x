{
module Sisyphus.Lexer

where

import Data.Char

import Control.Applicative ( Applicative(..) )
import Control.Monad ( liftM, ap )
import Data.List (break)
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

$special  = [\^\.\:\;\,\$\@\|\*\+\?\~\-\{\}\(\)\[\]\/\<\>\=\!\"]

$all_but_nl = [$idchar $special $white_no_nl]

@id         = $alpha $idchar*
@num        = $digit+
@comment    = "#".*
@arrow      = ("-"+ ("down" | "right" | "left" | "up"))? "-"+">"
@regionSep  = "-"+"-" | "|"+"|"
@short_note = "note" ("left"|"right"|"top"|"bottom") "of" $alphanum+ ":" $all_but_nl* $nl

tokens :-

        $white_no_nl+      { skip      }
        $nl                { vsemi `andBegin` bol }
        @comment           { skip      }
<nobol> @arrow             { arr       }
<bol>   @regionSep         { region    }
        "[*]"              { star `andBegin` nobol }
        "@startuml"        { startUml  }
        "@enduml"          { endUml    }
        $special           { special   }
<bol>   "state"            { state `andBegin` nobol }
<nobol> "<entry>"          { entry     }
<nobol> "<exit>"           { exit      }
<nobol> "<internal>"       { internal  }
<nobol> "<do>"             { doaction  }
<bol>   @id                { identNoteOrPragma }
<nobol> @id                { ident     }
        @num               { number    }

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
  | RegionSepT
  | StateT
  | EntryT
  | ExitT
  | DoActivityT
  | InternalT
  | IdT String
  | NumT String
  | VirtualSemiColonT
  | NoteStartT
  | NoteEndT
  | EOFT
  deriving Show

-- -----------------------------------------------------------------------------
-- Token functions

type Action = (AlexPosn,Char,String) -> Int -> P Token

special, startUml, endUml, arr, star, region, state :: Action
entry, exit, internal, ident, number, vsemi :: Action

special   (p,_,str) _  = return $ T p (SpecialT  (head str))
startUml  (p,_,_)   _  = return $ T p StartUmlT
endUml    (p,_,_)   _  = return $ T p EndUmlT
arr       (p,_,_)   _  = return $ T p ArrowT
star      (p,_,_)   _  = return $ T p StarStateT
region    (p,_,_)   _  = return $ T p RegionSepT
state     (p,_,_)   _  = return $ T p StateT
entry     (p,_,_)   _  = return $ T p EntryT
exit      (p,_,_)   _  = return $ T p ExitT
doaction  (p,_,_)   _  = return $ T p DoActivityT
internal  (p,_,_)   _  = return $ T p InternalT
ident     (p,_,str) ln = return $ T p (IdT (take ln str))
number    (p,_,str) ln = return $ T p (NumT (take ln str))
vsemi     (p,_,_)   _  = do pTkn <- getPrevToken
                            case pTkn of
                              StarStateT -> return $ T p VirtualSemiColonT
                              IdT _ -> return $ T p VirtualSemiColonT
                              NumT _ -> return $ T p VirtualSemiColonT
                              SpecialT c -> case c of
                                              ']' -> return $ T p VirtualSemiColonT
                                              '/' -> return $ T p VirtualSemiColonT
                                              _   -> lexToken
                              _ -> lexToken

skip :: Action
skip _ _ = lexToken

-- These rules are pretty crude, because they allow basically
-- everything after a note keyword as long as it is not a normal
-- word or number

isPragma :: Tkn -> Bool
isPragma tkn =
  case tkn of
    IdT  _    -> True
    NumT _    -> True
    otherwise -> False

skipLine :: (AlexPosn,Char,String) -> P Token
skipLine inp@(p,c,str) = do
  let
    (line,inp') = takeLine (p,c,[],str)
  setInput inp'
  setStartCode bol
  lexToken

skipPragmaOrElse :: (AlexPosn,Char,String) -> P Token -> P Token
skipPragmaOrElse inp cont = do
  nextTkn <- peekNextTkn
  if isPragma nextTkn
  then skipLine inp
  else cont

isNote :: Tkn -> Bool
isNote tkn =
  case tkn of
    IdT _        -> True
    SpecialT '"' -> True
    otherwise    -> False

skipNote :: (AlexPosn,Char,String) -> P Token
skipNote inp@(p,c,str) = do
  let
    (line,inp') = takeLine (p,c,[],str)
  if (('"' `elem` line) || (':' `elem` line)) -- this means a one line note
  then do
    setInput inp'
  else do
    let
      matchEndNote ("end":"note":_) = True
      matchEndNote _ = False
      isNoteEnd (line,inp) = (matchEndNote (words (line))) || (isInputEmpty inp)
      (_,inp'') = until isNoteEnd (takeLine . snd) (line,inp')
    setInput inp''
  setStartCode bol
  lexToken

skipNoteOrElse :: (AlexPosn,Char,String) -> P Token -> P Token
skipNoteOrElse inp cont = do
  nextTkn <- peekNextTkn
  if isNote nextTkn
  then skipNote inp
  else cont

identNoteOrPragma :: Action
identNoteOrPragma inp@(p,_,str) ln = do
  let firstWord = take ln str
      returnId = do setStartCode nobol; return $ T p (IdT firstWord)
  case firstWord of
    "scale" -> inp `skipPragmaOrElse` returnId
    "hide"  -> inp `skipPragmaOrElse` returnId
    "note"  -> inp `skipNoteOrElse` returnId
    otherwise -> returnId

-- -----------------------------------------------------------------------------
-- The input type

type Byte = Word8

type AlexInput = (AlexPosn,     -- current position,
                  Char,         -- previous char
                  [Byte],
                  String)       -- current input string

isInputEmpty :: AlexInput -> Bool
isInputEmpty (_,_,_,[]) = True
isInputEmpty _          = False

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

takeLine :: AlexInput -> (String,AlexInput)
takeLine inp = go [] inp
  where go cs inp =
          let line = reverse cs in
          case alexGetChar inp of
            Nothing -> (line,inp)
            Just (c,inp') -> if c == '\n' then (line,inp') else go (c:cs) inp'

-- -----------------------------------------------------------------------------
-- Token positions

-- `Posn' records the location of a token in the input text.  It has three
-- fields: the address (number of characters preceding the token), line number
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
type Scope = [String]

data PState = PState { startcode    :: Int
                     , input        :: AlexInput
                     , prevToken    :: Tkn
                     }
                     deriving (Show)

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

runP :: String -> P a -> Either ParseError (PState,a)
runP str (P p) = p initial_state
  where initial_state = PState{ startcode = 0
                              , input = (alexStartPos,'\n',[],str)
                              , prevToken = NoneT
                              }

failP :: String -> P a
failP str = P $ \PState{ input = (p,_,_,_) } -> Left (Just p,str)

getParserState :: P PState
getParserState = P $ \st -> Right (st, st)

setStartCode :: StartCode -> P ()
setStartCode sc = P $ \st -> Right (st{ startcode = sc }, ())

getStartCode :: P StartCode
getStartCode = P $ \st -> Right (st, startcode st)

getInput :: P AlexInput
getInput = P $ \st -> Right (st, input st)

setInput :: AlexInput -> P ()
setInput inp = P $ \st -> Right (st{ input = inp }, ())

getPrevToken :: P Tkn
getPrevToken = P $ \st -> Right (st, prevToken st)

setPrevToken :: Tkn -> P ()
setPrevToken t = P $ \st -> Right (st{ prevToken = t }, ())

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

peekNextTkn :: P Tkn
peekNextTkn = do
  st@(PState sc inp pTkn) <- getParserState
  let tkn = peekNextTkn' st inp sc
  case tkn of
    Left err -> lexError err
    Right t -> return t

peekNextTkn' st inp@(p,c,_,s) sc = do
  case alexScan inp sc of
    AlexEOF -> Right EOFT
    AlexError _ -> Left "lexical error"
    AlexSkip inp1 _ -> peekNextTkn' st inp1 sc
    AlexToken inp1 len a ->
      case unP (a (p,c,s) len) st of
        Left (p,s) -> Left s
        Right (st',t@(T p tkn)) -> Right tkn

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
