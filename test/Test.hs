module Main where

import Control.Exception (evaluate)
import Scanner
import Test.Hspec

crng x0 y0 x1 y1 = CodeRng (CodeLoc x0 y0) (CodeLoc x1 y1)

crng1 y0 = crng 1 y0 1

main :: IO ()
main = hspec $ do
  describe "Scanner" $ do
    it "returns EOF for empty input" $ do
      scan "" `shouldBe` (Right (), [Token "" (crng1 1 1) EOF])

    it "skip comment" $ do
      scan "\t\n  //hello world" `shouldBe` (Right (), [Token "" (crng 2 16 2 16) EOF])

    it "reject unexpected character" $ do
      scan "_invalid" `shouldBe` (Left $ ScanErr "unexpected char at 1:1", [])

    it "identify string literal and escape" $ do
      {- "\bhe\"llo\n"
       - ""
       -}
      let raw = "\"\\bhe\\\"llo\\n\"\n\"\""
      scan raw
        `shouldBe` ( Right (),
                     [ Token "\"\\bhe\\\"llo\\n\"" (crng1 1 13) STRING,
                       Token "\"\"" (crng 2 1 2 2) STRING,
                       Token "" (crng 2 3 2 3) EOF
                     ]
                   )

    it "report error for unsupported escape sequence" $ do
      let raw = "\"\\m\"" -- "\m"
      scan raw `shouldBe` (Left $ ScanErr "cannot escape m at 1:3", [])

    it "give error for unclosed string" $ do
      let raw = "\"unclosed string literal" -- "unclosed string literal ends at 24, EOF 25
      scan raw `shouldBe` (Left $ ScanErr "unexpected EOF at 1:25", [])

    it "find numerical tokens && math operators" $ do
      scan "123. >= 0.5  7 < 4 + 10>!.,;/-{}(*)!===<="
        `shouldBe` ( Right (),
                     [ Token "123." (crng1 1 4) NUMBER,
                       Token ">=" (crng1 6 7) GREATER_EQUAL,
                       Token "0.5" (crng1 9 11) NUMBER,
                       Token "7" (crng1 14 14) NUMBER,
                       Token "<" (crng1 16 16) LESS,
                       Token "4" (crng1 18 18) NUMBER,
                       Token "+" (crng1 20 20) PLUS,
                       Token "10" (crng1 22 23) NUMBER,
                       Token ">" (crng1 24 24) GREATER,
                       Token "!" (crng1 25 25) BANG,
                       Token "." (crng1 26 26) DOT,
                       Token "," (crng1 27 27) COMMA,
                       Token ";" (crng1 28 28) SEMICOLON,
                       Token "/" (crng1 29 29) SLASH,
                       Token "-" (crng1 30 30) MINUS,
                       Token "{" (crng1 31 31) LEFT_BRACE,
                       Token "}" (crng1 32 32) RIGHT_BRACE,
                       Token "(" (crng1 33 33) LEFT_PAREN,
                       Token "*" (crng1 34 34) STAR,
                       Token ")" (crng1 35 35) RIGHT_PAREN,
                       Token "!=" (crng1 36 37) BANG_EQUAL,
                       Token "==" (crng1 38 39) EQUAL_EQUAL,
                       Token "<=" (crng1 40 41) LESS_EQUAL,
                       Token "" (crng1 42 42) EOF
                     ]
                   )

    it "recognize keywords" $ do
      let raw = "class else false for fun if nil not or print return super this true var while"
      scan raw
        `shouldBe` ( Right (),
                     [ Token "class" (crng1 1 5) CLASS,
                       Token "else" (crng1 7 10) ELSE,
                       Token "false" (crng1 12 16) FALSE,
                       Token "for" (crng1 18 20) FOR,
                       Token "fun" (crng1 22 24) FUN,
                       Token "if" (crng1 26 27) IF,
                       Token "nil" (crng1 29 31) NIL,
                       Token "not" (crng1 33 35) NOT,
                       Token "or" (crng1 37 38) OR,
                       Token "print" (crng1 40 44) PRINT,
                       Token "return" (crng1 46 51) RETURN,
                       Token "super" (crng1 53 57) SUPER,
                       Token "this" (crng1 59 62) THIS,
                       Token "true" (crng1 64 67) TRUE,
                       Token "var" (crng1 69 71) VAR,
                       Token "while" (crng1 73 77) WHILE,
                       Token "" (crng1 78 78) EOF
                     ]
                   )

    it "distinguish normal identifers and keywords && track code location" $ do
      {-
       and andd 	 not
       NOT
       -}
      let raw = "and andd \t not\nNOT"
      scan raw
        `shouldBe` ( Right (),
                     [ Token "and" (crng 1 1 1 3) AND,
                       Token "andd" (crng 1 5 1 8) IDENTIFIER,
                       Token "not" (crng 1 12 1 14) NOT,
                       Token "NOT" (crng 2 1 2 3) IDENTIFIER,
                       Token "" (crng 2 4 2 4) EOF
                     ]
                   )

  describe "Parser" $ do return ()

  describe "Eval" $ do return ()
