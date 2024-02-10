module Main where

import Control.Monad (mapM_)
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
  let (res, toks) = Scanner.scan r
  case res of
    Left err -> putStrLn $ "Out> ERROR: " ++ Scanner.reason err
    _ -> putStrLn "Out> Tokenization ok."
  mapM_ print toks
  repl
