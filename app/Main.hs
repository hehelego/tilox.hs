module Main where

import qualified AST
import Control.Monad (mapM_)
import qualified NaiveEval as Eval
import qualified Parser
import qualified Scanner
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.IO

main :: IO ()
main = do
  args <- getArgs
  case args of
    fp : _ -> readFile fp >>= exec
    [] -> repl Eval.emptyEnv

exec :: String -> IO ()
exec source = do
  let (res, toks) = Scanner.scan source
  toks <- case res of
    Left err -> putStrLn ("Scanner error: " ++ Scanner.reason err) >> exitFailure
    _ -> pure toks

  let (res, _) = Parser.runParser Parser.progP toks
  prog <- case res of
    Left err -> putStrLn ("Parser error: " ++ Parser.errMsg err) >> exitFailure
    Right prog -> pure prog

  (res, _) <- Eval.runState (Eval.runProg prog) Eval.emptyEnv
  case res of
    Left err -> putStrLn ("Execution error: " ++ show err) >> exitFailure
    Right _ -> exitSuccess

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
