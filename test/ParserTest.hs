module ParserTest (test) where

import Parser
import qualified Scanner as S
import Test.Hspec

ateof rest = do
  length rest `shouldBe` 1
  let eof = head rest
  S.tokType eof `shouldBe` S.EOF

test :: Spec
test = do
  describe "Parser-Expr" $ do
    it "parse simply addition" $ do
      let (Right (), toks) = S.scan "1+1"
      let (Right expr, rest) = runParser exprP toks
      ateof rest
      show expr `shouldBe` "(1.0 + 1.0)"

    it "parse complex mul add - 1" $ do
      let (Right (), toks) = S.scan "((1.0 + 1.0) + (1.0 * 2.0))"
      let (Right expr, rest) = runParser exprP toks
      ateof rest
      show expr `shouldBe` "((1.0 + 1.0) + (1.0 * 2.0))"

    it "parse complex mul add - 2" $ do
      let (Right (), toks) = S.scan "1+1*2"
      let (Right expr, rest) = runParser exprP toks
      ateof rest
      show expr `shouldBe` "(1.0 + (1.0 * 2.0))"

    it "parse complex mul add - 3" $ do
      let (Right (), toks) = S.scan "1*1*1+2"
      let (Right expr, rest) = runParser exprP toks
      ateof rest
      show expr `shouldBe` "(((1.0 * 1.0) * 1.0) + 2.0)"

  describe "Parser-Stmt" $ do
    pure ()
