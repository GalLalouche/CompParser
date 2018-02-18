module Main where

import Parsers
import Printers
import Common.Operators
import Data.Maybe(fromJust)
import System.Environment(getArgs)
import qualified Text.Parsec as Parsec
import Common.Foldables

findOptionsStart :: String -> String
findOptionsStart = lines .> dropWhile ("OPTIONS" /=) .> drop 2 .> unlines

main :: IO ()
main = do
  args <- getArgs
  let commandName = head args
  let flag = nth 1 args
--   lines <- findOptionsStart <$> getContents
  input <- getContents
--   let parsedCommands = printCommands commandName <$> Parsec.parse parseCommands "(input)" input
--   let parsedCommands = printArguments commandName <$> Parsec.parse parseArguments "(input)" lines
  let parsedCommands = printSimpleArguments commandName <$> Parsec.parse parseSimpleArguments "(input)" input
  case parsedCommands of
    Left e -> error $ "failed parsing " ++ show e
    Right cs -> putStrLn cs
