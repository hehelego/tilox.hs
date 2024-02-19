module ParserTest (test) where

import Parser
import qualified Scanner as S
import Test.Hspec

ateof rest = do
  length rest `shouldBe` 1
  let eof = head rest
  S.tokType eof `shouldBe` S.EOF

parseE input = do
  let (Right (), toks) = S.scan input
  let (Right expr, rest) = runParser exprP toks
  ateof rest
  pure expr

parseE' input = do
  let (Right (), toks) = S.scan input
  let (Left e, _) = runParser exprP toks
  pure e

test :: Spec
test = do
  describe "Parser-Expr" $ do
    it "parse primary expressions - number" $ do
      expr <- parseE "1"
      show expr `shouldBe` "1.0"
      expr <- parseE "1."
      show expr `shouldBe` "1.0"
      expr <- parseE "0."
      show expr `shouldBe` "0.0"
      expr <- parseE "0993"
      show expr `shouldBe` "993.0"

    it "parse primary expressions - string" $ do
      let r = "aa"
      expr <- parseE $ show r
      show expr `shouldBe` show r
      let r = "a\t\n\\a"
      expr <- parseE $ show r
      show expr `shouldBe` show r

    it "parse primary expressions - bool" $ do
      expr <- parseE "true"
      show expr `shouldBe` "true"
      expr <- parseE "false"
      show expr `shouldBe` "false"

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
