module Main where

import Control.Exception (evaluate)
import Scanner
import Test.Hspec

crng x0 y0 x1 y1 = CodeRng (CodeLoc x0 y0) (CodeLoc x1 y1)

main :: IO ()
main = hspec $ do
  describe "Scanner" $ do
    it "returns EOF for empty input" $ do
      scan "" `shouldBe` (Right (), [Token "" (crng 1 1 1 1) EOF])

    it "identify string literal and escape" $ do
      let raw = "\"\\bhe\\\"llo\\n\"" -- "\bhe\"llo\n"
      scan raw
        `shouldBe` ( Right (),
                     [ Token "\"\bhe\"llo\n\"" (crng 1 1 1 13) STRING,
                       Token "" (crng 1 14 1 14) EOF
                     ]
                   )

    it "give error for unclosed string" $ do
      let raw = "\"unclosed string literal" -- "unclosed string literal ends at 24, EOF 25
      scan raw `shouldBe` (Left $ ScanErr "unexpected EOF at 1:25", [])

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
