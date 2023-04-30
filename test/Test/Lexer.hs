module Test.Lexer where

import Djot.Lexer

import Test.Tasty
import Test.Tasty.HUnit

spec :: TestTree
spec =
  testGroup
    "Lexing"
    [ testCase "Lex a number" lexNumber
    , testCase "Lex a alphanum characters" lexAlphaNum
    , testGroup
        "Lex special characters"
        [ testCase "Lex 3 different characters" lexSpecialChars
        , testCase "Lex 3 hashes" lexThreeHashes
        ]
    , testCase "Lex a mixed string" lexMixedString
    ]

lexNumber :: IO ()
lexNumber =
  scanMany "32"
    @?= Right
      [ RangedToken{rtToken = Digit "32", rtRange = Range{start = AlexPn 0 1 1, stop = AlexPn 2 1 3}}
      , RangedToken{rtToken = EOF, rtRange = Range{start = AlexPn 2 1 3, stop = AlexPn 2 1 3}}
      ]

lexAlphaNum :: IO ()
lexAlphaNum =
  scanMany "abc"
    @?= Right
      [ RangedToken{rtToken = String "abc", rtRange = Range{start = AlexPn 0 1 1, stop = AlexPn 3 1 4}}
      , RangedToken{rtToken = EOF, rtRange = Range{start = AlexPn 3 1 4, stop = AlexPn 3 1 4}}
      ]

lexSpecialChars :: IO ()
lexSpecialChars =
  scanMany "#^."
    @?= Right
      [ RangedToken{rtToken = TokHash, rtRange = Range{start = AlexPn 0 1 1, stop = AlexPn 1 1 2}}
      , RangedToken{rtToken = TokCircumflex, rtRange = Range{start = AlexPn 1 1 2, stop = AlexPn 2 1 3}}
      , RangedToken{rtToken = TokDot, rtRange = Range{start = AlexPn 2 1 3, stop = AlexPn 3 1 4}}
      , RangedToken{rtToken = EOF, rtRange = Range{start = AlexPn 3 1 4, stop = AlexPn 3 1 4}}
      ]

lexThreeHashes :: IO ()
lexThreeHashes =
  scanMany "###"
    @?= Right
      [ RangedToken{rtToken = TokHash, rtRange = Range{start = AlexPn 0 1 1, stop = AlexPn 1 1 2}}
      , RangedToken{rtToken = TokHash, rtRange = Range{start = AlexPn 1 1 2, stop = AlexPn 2 1 3}}
      , RangedToken{rtToken = TokHash, rtRange = Range{start = AlexPn 2 1 3, stop = AlexPn 3 1 4}}
      , RangedToken{rtToken = EOF, rtRange = Range{start = AlexPn 3 1 4, stop = AlexPn 3 1 4}}
      ]

lexMixedString :: IO ()
lexMixedString =
  scanMany "[MyString]"
    @?= Right
      [ RangedToken{rtToken = TokOpenSquareBracket, rtRange = Range{start = AlexPn 0 1 1, stop = AlexPn 1 1 2}}
      , RangedToken{rtToken = String "MyString", rtRange = Range{start = AlexPn 1 1 2, stop = AlexPn 9 1 10}}
      , RangedToken{rtToken = TokCloseSquareBracket, rtRange = Range{start = AlexPn 9 1 10, stop = AlexPn 10 1 11}}
      , RangedToken{rtToken = EOF, rtRange = Range{start = AlexPn 10 1 11, stop = AlexPn 10 1 11}}
      ]
