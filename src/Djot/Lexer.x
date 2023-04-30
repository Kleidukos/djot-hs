{
{-# LANGUAGE OverloadedStrings #-}
module Djot.Lexer where

import Prelude hiding (lex)
import Data.ByteString.Lazy.Char8 qualified as BS
import Data.ByteString.Lazy (LazyByteString)
}

%wrapper "monadUserState-bytestring"

$whitechar = [\t\n\r\f\v\ ]
$large = [A-Z \xc0-\xd6 \xd8-\xde]
$small = [a-z \xdf-\xf6 \xf8-\xff \_]
$alpha = [$small $large]
$digit = 0-9
$special = [\(\)\[\]\`\{\}\=\#\>\.\+\*\-\:\^\|]


djot :-

<0> $white+    { skip }
<0> $digit     { mkL TokTypeInteger }
<0> $alpha     { mkL TokTypeInteger }
<0> $special   { mkL TokTypeSpecial }

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
  | TokRightParen
  | TokLeftParen
  | TokComma
  | TokSemicolon
  | TokLeftSquareBracket
  | TokRightSquareBracket
  | TokLeftAccolade
  | TokRightAccolade
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
mkL tokType (start, _, str, _) len =
  refineTok tokType start str len 

mkRange :: AlexPosn -> LazyByteString -> Int64 -> Range
mkRange start str len = Range{start, stop}
  where
    stop :: AlexPosn
    stop = BS.foldl' alexMove start $ BS.take len str

refineTok :: TokenType -> AlexPosn -> LazyByteString -> Int64 -> Alex RangedToken
refineTok tokenType start str len =
  case tokenType of
    TokTypeString  -> pure RangedToken{rtToken = String str, rtRange = mkRange start str len}
    TokTypeInteger -> pure RangedToken{rtToken = Digit str, rtRange = mkRange start str len}
    TokTypeSpecial -> pure RangedToken{rtToken = mkTokSpecial str, rtRange = mkRange start str len}
    TokTypeEOF      -> alexEOF

mkTokSpecial :: LazyByteString -> Token
mkTokSpecial = \case
  ")" -> TokRightParen
  "(" -> TokLeftParen
  "," -> TokComma
  ";" -> TokSemicolon
  "[" -> TokRightSquareBracket
  "]" -> TokLeftSquareBracket
  "{" -> TokLeftAccolade
  "}" -> TokRightAccolade
  "`" -> TokBacktick
  "|" -> TokPipe
  ">" -> TokGreaterThan
  "=" -> TokEqual
  "#" -> TokHash
  "." -> TokDot
  "+" -> TokPlus
  "*" -> TokTimes
  "-" -> TokMinus
  ":" -> TokColon
  "^" -> TokCircumflex

scanMany :: LazyByteString -> Either String [RangedToken]
scanMany input = runAlex input go
  where
    go = do
      output <- alexMonadScan
      if rtToken output == EOF
        then pure [output]
        else (output :) <$> go
}
