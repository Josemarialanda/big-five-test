module Main where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Aeson (Value, encode)
import Data.Text ()
import Network.HTTP.Req
  ( POST (POST),
    ReqBodyJson (ReqBodyJson),
    defaultHttpConfig,
    https,
    jsonResponse,
    req,
    responseBody,
    runReq,
    (/:),
  )
import Parser (big5reportParser)
import System.Environment (getArgs)
import System.Exit
  ( exitFailure,
    exitSuccess,
  )
import Types
  ( Parser (runParser),
    Results (results'email, results'name),
  )

main :: IO ()
main = getArgs >>= run

run :: [String] -> IO ()
run ["-h"] = usage >> exit
run ["--help"] = usage >> exit
run ["-v"] = version >> exit
run ["--version"] = usage >> exit
run
  [ "--upload",
    file,
    name,
    email
    ] = exec uploadAction file name email
run
  [ file,
    name,
    email
    ] = exec displayAction file name email >> exit
run _ = putStrLn "Not a valid command" >> die

exec :: (Results -> IO ()) -> FilePath -> String -> String -> IO ()
exec action filename name email = readFile filename >>= either error action . runParser big5reportParser

displayAction :: Results -> IO ()
displayAction = print

uploadAction :: Results -> IO ()
uploadAction payload = runReq defaultHttpConfig $ do
  response <-
    req
      POST
      (https "recruiting.bellroy.com" /: "api" /: "v1" /: "roles" /: "bellroy-tech-team-recruit" /: "big_five_profile_submissions")
      (ReqBodyJson payload)
      jsonResponse
      mempty
  liftIO $ print (responseBody response :: Value)

usage :: IO ()
usage = putStrLn "Usage: big-five-test [file] [name] [email]"

version :: IO ()
version = putStrLn "big-five-test 0"

exit :: IO a
exit = exitSuccess

die :: IO a
die = exitFailure
