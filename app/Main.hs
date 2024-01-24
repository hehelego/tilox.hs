module Main where

import System.Environment (getArgs)
import System.Exit
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
  r <- getLine
  putStrLn $ "> " ++ r
  repl
