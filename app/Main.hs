module Main where

import Control.Monad (forM_)
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
  putStr "> "
  hFlush stdout
  r <- getLine
  case Scanner.scan Scanner.start r of
    Right sc' -> case Scanner.stop sc' of
      Left err -> putStrLn $ "ERROR: " ++ show err
      Right tokens -> forM_ tokens print
    Left err -> putStrLn $ "ERROR: " ++ show err
  repl
