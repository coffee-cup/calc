module Main where

import Eval
import Parser

import Control.Monad.Trans
import System.Console.Haskeline
import Text.Printf

process :: String -> IO ()
process line = do
  let res = parseExpr line
  case res of
    Left err -> print err
    Right ex -> case eval ex of
      Nothing -> putStrLn "Cannot evaluate"
      Just result -> putStrLn $ printf "%.2f" result

main :: IO ()
main = runInputT defaultSettings loop
      where
        loop = do
          minput <- getInputLine "calc> "
          case minput of
            Nothing -> outputStrLn "Goodbye."
            Just input -> liftIO (process input) >> loop
