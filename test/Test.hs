module Main where

import Control.Exception (evaluate)
import qualified ParserTest
import qualified ScannerTest
import Test.Hspec

main :: IO ()
main = hspec $ do
  ScannerTest.test
  ParserTest.test
