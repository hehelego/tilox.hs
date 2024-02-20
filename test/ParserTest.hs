module ParserTest (test) where

import qualified Parser as P
import qualified Scanner as S
import Test.Hspec

ateof rest = do
  length rest `shouldBe` 1
  let eof = head rest
  S.tokType eof `shouldBe` S.EOF

parseE input = do
  let (Right (), toks) = S.scan input
  let (Right expr, rest) = P.runParser P.exprP toks
  ateof rest
  pure expr

parseE' input = do
  let (Right (), toks) = S.scan input
  let (Left e, _) = P.runParser P.exprP toks
  pure e

hasPrefix :: (Eq a) => [a] -> [a] -> Bool
hasPrefix [] _ = True
hasPrefix (_ : _) [] = False
hasPrefix (x : xs) (y : ys) = x == y && xs `hasPrefix` ys

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
      expr <- parseE "(0993 )"
      show expr `shouldBe` "993.0"

    it "parse primary expressions - string" $ do
      let r = "aa"
      expr <- parseE $ show r
      show expr `shouldBe` show r
      let r = "a\t\n\\a"
      expr <- parseE $ show r
      show expr `shouldBe` show r
      let r = "(  a\t\n\\a)"
      expr <- parseE $ show r
      show expr `shouldBe` show r

    it "parse primary expressions - bool" $ do
      expr <- parseE "true"
      show expr `shouldBe` "true"
      expr <- parseE "false"
      show expr `shouldBe` "false"
      expr <- parseE "(false)"
      show expr `shouldBe` "false"

    it "parse primary expressions - nil" $ do
      expr <- parseE "nil"
      show expr `shouldBe` "nil"
      expr <- parseE "\t(nil\t )\t"
      show expr `shouldBe` "nil"

    it "parse unary expression" $ do
      expr <- parseE "-1"
      show expr `shouldBe` "(- 1.0)"
      expr <- parseE "-true"
      show expr `shouldBe` "(- true)"
      expr <- parseE "-\"a bad expression\""
      show expr `shouldBe` "(- \"a bad expression\")"
      expr <- parseE "! nil"
      show expr `shouldBe` "(! nil)"
      expr <- parseE "!538."
      show expr `shouldBe` "(! 538.0)"
      expr <- parseE "--1"
      show expr `shouldBe` "(- (- 1.0))"
      expr <- parseE "-!true"
      show expr `shouldBe` "(- (! true))"
      expr <- parseE "!!false"
      show expr `shouldBe` "(! (! false))"

    it "parse incomplete unary expression" $ do
      err <- parseE' "!!!"
      P.errMsg err `shouldSatisfy` hasPrefix "no unary expression"
      err <- parseE' "--!--!"
      P.errMsg err `shouldSatisfy` hasPrefix "no unary expression"

    it "parse binary expression" $ do
      expr <- parseE "1 + 2"
      show expr `shouldBe` "(1.0 + 2.0)"
      expr <- parseE "0.3 + -2"
      show expr `shouldBe` "(0.3 + (- 2.0))"
      expr <- parseE "5352352 - 124124"
      show expr `shouldBe` "(5352352.0 - 124124.0)"
      expr <- parseE "nil * \"hello world\""
      show expr `shouldBe` "(nil * \"hello world\")"
      expr <- parseE "true / (nil * 5.0)"
      show expr `shouldBe` "(true / (nil * 5.0))"

    -- TODO: find suitable way to test error handling
    it "parse incomplete binary expression" $ do
      err <- parseE' "1 + "
      P.errMsg err `shouldSatisfy` const False
      err <- parseE' "+"
      show err `shouldSatisfy` const False
      err <- parseE' "3 - 5 * "
      show err `shouldSatisfy` const False
      err <- parseE' "nil * "
      show err `shouldSatisfy` const False
      err <- parseE' "true / (nil * 5.0"
      show err `shouldSatisfy` const False

    -- TODO: fuzzing testing?
    it "parse complex expression, handling precedence" $ do
      expr <- parseE "1 + 2 - 3 + 4 - 5"
      show expr `shouldBe` "((((1.0 + 2.0) - 3.0) + 4.0) - 5.0)"
      expr <- parseE "1 + 2 * 3 / 4"
      show expr `shouldBe` "(1.0 + ((2.0 * 3.0) / 4.0))"
      expr <- parseE "(1 + 2) * 3 / 4"
      show expr `shouldBe` "(((1.0 + 2.0) * 3.0) / 4.0)"

    -- TODO: program,statement,function/variable/class parsing
    describe "Parser-Stmt" $ do
      pure ()
