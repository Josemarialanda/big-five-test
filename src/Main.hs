module Main where

import System.Environment ( getArgs )
import Data.Aeson ( encode )

import Types
    ( Results(results'email, results'name), Parser(runParser) ) 
import Parser (big5reportParser)
import Data.Text ()


main :: IO ()
main = do
  (filename:name:email:_) <- getArgs
  testResultFile          <- readFile filename
  either error (print . encode) (runParser big5reportParser testResultFile)