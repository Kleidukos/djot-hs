module Main (main) where

import Test.Tasty

import Test.Lexer qualified as LexerTest

main :: IO ()
main = do
  defaultMain $ testGroup "djot-hs tests" specs

specs :: [TestTree]
specs =
  [ LexerTest.spec
  ]
