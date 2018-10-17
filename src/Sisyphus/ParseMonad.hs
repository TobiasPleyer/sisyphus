module Sisyphus.ParseMonad (
        AlexInput, alexInputPrevChar, alexGetChar, alexGetByte,
        AlexPosn(..), alexStartPos,

        P, runP, failP, getInput, setInput, StartState(..),
        setDefaultStartState, setExplicitStartState, getStartState,
        StartCode, setStartCode, getStartCode
 ) where

import Control.Applicative ( Applicative(..) )
import Control.Monad ( liftM, ap )
import Data.Word (Word8)
import qualified Sisyphus.Util as Util

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

data StartState a = NoStartState
                  | DefaultStartState a
                  | ExplicitStartState a
                  deriving (Show)

data PState = PState { start_state :: StartState String
                     , startcode   :: Int
                     , input       :: AlexInput
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
 where initial_state = PState{ start_state=NoStartState
                             , startcode = 0
                             , input=(alexStartPos,'\n',[],str)
                             }

failP :: String -> P a
failP str = P $ \PState{ input = (p,_,_,_) } -> Left (Just p,str)

setDefaultStartState :: String -> P ()
setDefaultStartState state = P $ \s -> Right (s{ start_state = DefaultStartState state }, ())

setExplicitStartState :: String -> P ()
setExplicitStartState state = P $ \s -> Right (s{ start_state = ExplicitStartState state }, ())

getStartState :: P (StartState String)
getStartState = P $ \s -> Right (s, start_state s)

setStartCode :: StartCode -> P ()
setStartCode sc = P $ \s -> Right (s{ startcode = sc }, ())

getStartCode :: P StartCode
getStartCode = P $ \s -> Right (s, startcode s)

getInput :: P AlexInput
getInput = P $ \s -> Right (s, input s)

setInput :: AlexInput -> P ()
setInput inp = P $ \s -> Right (s{ input = inp }, ())
