module Main where

import qualified AST
import Control.Monad (mapM_)
import qualified NaiveEval as Eval
import qualified Parser
import qualified Scanner
import System.Environment (getArgs)
import System.IO

main :: IO ()
main = do
  args <- getArgs
  case args of
    fp : _ -> readFile fp >>= exec
    [] -> repl Eval.emptyEnv

exec :: String -> IO ()
exec source = undefined

repl :: Eval.Env -> IO ()
repl env = do
  putStr "In> "
  hFlush stdout
  r <- getLine
  putStrLn "=== tokenization ==="
  let (res, toks) = Scanner.scan r
  case res of
    Left err -> putStrLn $ "ERROR: " ++ Scanner.reason err
    _ -> pure ()
  putStrLn "=== parsing ==="
  let (res, toks') = Parser.runParser Parser.progP toks
  case res of
    Left err -> do
      putStrLn $ "ERROR: " ++ Parser.errMsg err
      repl env
    Right prog@(AST.Prog stmts) -> do
      mapM_ print stmts
      putStrLn "=== evaluating ==="
      (e, env') <- Eval.runState (Eval.runProg prog) env
      putStrLn "=== execution result ==="
      print e
      repl env'
