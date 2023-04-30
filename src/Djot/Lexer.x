{
{-# LANGUAGE OverloadedStrings #-}
module Djot.Lexer where

import Prelude hiding (lex)
import Data.ByteString.Lazy.Char8 qualified as BS
import Data.ByteString.Lazy (LazyByteString)
}

%wrapper "monadUserState-bytestring"

$whitechar = [\t\n\r\f\v\ ]
$large = [A-Z]
$small = [a-z]
$alpha = [$small $large]
$digit = 0-9
$special = [\(\)\[\]\`\{\}\=\#\>\.\+\*\-\:\^\|]

@number = [$digit]+
@text = [$alpha $digit]

djot :-

<0> $white+     { skip }
<0> @number     { mkL TokTypeInteger }
<0> $special    { mkL TokTypeSpecial }
<0> $alpha+     { mkL TokTypeString }

{
data AlexUserState = AlexUserState

alexInitUserState = AlexUserState

alexEOF :: Alex RangedToken
alexEOF = do
  (pos, _, _, _) <- alexGetInput
  pure $ RangedToken EOF (Range pos pos)

data Range = Range
  { start :: AlexPosn
  , stop :: AlexPosn
  } deriving stock (Eq, Show)

data RangedToken = RangedToken
  { rtToken :: Token
  , rtRange :: Range
  } deriving stock (Eq, Show)

data TokenType
  = TokTypeString
  | TokTypeInteger
  | TokTypeSpecial
  | TokTypeEOF
 deriving stock (Eq, Show)

data Token
  = Digit !LazyByteString
  | String !LazyByteString 
  -- "Special"
  | TokOpenParen
  | TokCloseParen
  | TokComma
  | TokSemicolon
  | TokOpenSquareBracket
  | TokCloseSquareBracket
  | TokOpenAccolade
  | TokCloseAccolade
  | TokBacktick
  | TokPipe
  | TokGreaterThan
  | TokEqual
  | TokHash
  | TokDot
  | TokPlus
  | TokTimes
  | TokMinus
  | TokColon
  | TokCircumflex
  | EOF
 deriving stock (Eq, Show)

mkL :: TokenType -> AlexInput -> Int64 -> Alex RangedToken
mkL tokenType (start, _, str, _) len =
  case tokenType of
    TokTypeString  -> pure RangedToken{rtToken = String str, rtRange = mkRange start str len}
    TokTypeInteger -> pure RangedToken{rtToken = Digit str, rtRange = mkRange start str len}
    TokTypeSpecial -> pure RangedToken{rtToken = mkTokSpecial (BS.head str), rtRange = mkRange start str len}
    TokTypeEOF      -> alexEOF

mkRange :: AlexPosn -> LazyByteString -> Int64 -> Range
mkRange start str len = Range{start, stop}
  where
    stop :: AlexPosn
    stop = BS.foldl' alexMove start $ BS.take len str

mkTokSpecial :: Char -> Token
mkTokSpecial = \case
  ')' -> TokCloseParen
  '(' -> TokOpenParen
  ',' -> TokComma
  ';' -> TokSemicolon
  '[' -> TokOpenSquareBracket
  ']' -> TokCloseSquareBracket
  '{' -> TokOpenAccolade
  '}' -> TokCloseAccolade
  '`' -> TokBacktick
  '|' -> TokPipe
  '>' -> TokGreaterThan
  '=' -> TokEqual
  '.' -> TokDot
  '+' -> TokPlus
  '*' -> TokTimes
  '-' -> TokMinus
  ':' -> TokColon
  '^' -> TokCircumflex
  '#' -> TokHash

scanMany :: LazyByteString -> Either String [RangedToken]
scanMany input = runAlex input go
  where
    go = do
      output <- alexMonadScan
      if rtToken output == EOF
        then pure [output]
        else (output :) <$> go
}
