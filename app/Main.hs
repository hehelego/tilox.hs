module Main where

import Control.Monad (mapM_)
import qualified Parser
import qualified Scanner
import System.Environment (getArgs)
import System.IO

main :: IO ()
main = do
  args <- getArgs
  case args of
    fp : _ -> readFile fp >>= exec
    [] -> repl

exec :: String -> IO ()
exec source = undefined

repl :: IO ()
repl = do
  putStr "In> "
  hFlush stdout
  r <- getLine
  putStrLn "=== tokenization ==="
  let (res, toks) = Scanner.scan r
  case res of
    Left err -> putStrLn $ "ERROR: " ++ Scanner.reason err
    _ -> pure ()
  putStrLn "=== parsing ==="
  let (res, toks') = Parser.runParser Parser.exprP toks
  case res of
    Left err -> putStrLn $ "ERROR: " ++ Parser.errMsg err
    Right e -> print e
  repl
