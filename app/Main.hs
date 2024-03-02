module Main where

import qualified AST
import Control.Monad (mapM_)
import Data.Functor (($>))
import qualified NaiveEval as Eval
import qualified Parser
import qualified Scanner
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.IO (hFlush, stdout)

main :: IO ()
main = do
  args <- getArgs
  case args of
    fp : _ -> readFile fp >>= exec
    [] -> repl Eval.initEnv

exec :: String -> IO ()
exec source = do
  let (res, toks) = Scanner.scan source
  toks <- case res of
    Left err -> putStrLn ("Scanner error: " ++ Scanner.reason err) >> exitFailure
    _ -> pure toks

  let (res, prog) = Parser.parse toks
  prog <- case res of
    Left err -> putStrLn ("Parser error: " ++ Parser.errMsg err) >> exitFailure
    _ -> pure prog

  (res, _) <- Eval.run Eval.initEnv prog
  case res of
    Left err -> putStrLn ("Execution error: " ++ show err) >> exitFailure
    _ -> exitSuccess

repl :: Eval.Env -> IO ()
repl env = do
  input <- putStr "In> " >> hFlush stdout >> getLine

  putStrLn "=== tokenizing ==="
  let (res, toks) = Scanner.scan input
  case res of
    Left err -> putStrLn $ "ERROR: " ++ Scanner.reason err
    _ -> pure ()

  putStrLn "=== parsing ==="
  let (res, prog) = Parser.parse toks
  case res of
    Left err -> putStrLn $ "ERROR: " ++ Parser.errMsg err
    _ -> pure ()

  print prog >> putStrLn "=== executing ==="
  (res, env') <- Eval.run env prog
  putStrLn "=== result ===" >> print res
  repl env'
