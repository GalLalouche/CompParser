{-# LANGUAGE DuplicateRecordFields #-}
module LibInternal where

import Common.Operators
import Common.Parsec
import Control.Applicative
import Data.Char(isSpace)
import Data.Either(lefts)
import Data.Functor
import Data.List(intercalate)
import Data.Maybe
import Debug.Trace
import Text.Format
import Text.Parsec((<?>))
import Text.Printf
import qualified Text.Parsec as P

data Command = Command {
    name :: String
  , desc :: String
} deriving (Eq, Ord, Show)

type Commands = [Command]
-- The number of spaces from the start of the line (^) to the first letter of the description
type TabWidth = Int
type CommandName = String

trim :: String -> String
trim = reverse .> dropWhile isSpace .> reverse

parseName :: Parser String
parseName = P.many1 nonSpace <?> "name"

parseTabWidth :: Parser TabWidth
parseTabWidth = let
    spaces = P.many lineSpace
    parser = do
      preSpaces <- spaces
      name <- parseName
      postSpaces <- spaces
      return $ length $ preSpaces ++ name ++ postSpaces
  in parser <?> "tab-width"

parseCommand :: TabWidth -> Parser Command
parseCommand tabWidth = let
    descriptionParser = (P.many1 (P.many1 nonEndLine <|> endLineAndTab) <$$> concat <$$> trim) <?> "description" where
      endLineAndTab :: Parser String
      endLineAndTab = P.try (P.endOfLine *> tab *> nonSpace) <$$> (\s -> [' ', s])
      tab :: Parser ()
      tab = parseAtLeast tabWidth lineSpace $> ()
    name = lineSpaces *> parseName
    description = lineSpaces *> descriptionParser
    command = liftA2 Command name description
  in command <?> "Command"

parseCommands :: Parser Commands
parseCommands = do
  tabWidth <- P.lookAhead parseTabWidth <?> "finding tab width"
  let lineParser = parseLine tabWidth
  parsedLines <- P.sepEndBy1 lineParser P.endOfLine <* P.eof
  return $ lefts parsedLines where
    parseLine tabWidth = parseEither (P.try $ parseCommand tabWidth) parseEmpty
    parseEmpty :: Parser ()
    parseEmpty = lineSpaces <?> "empty"

printCommands :: CommandName -> Commands -> String
printCommands commandName subCommands = format lines [commandName] where
  lines = unlines $ [
        "#compdef {0}"
      , ""
      , "_{0}() {"
      , "  _arguments '1: :__{0}_subcommands'"
      , "}"
      , ""
      , "__{0}_subcommands () {"
      , "  _values 'subcommand' \\"] ++
      map toString subCommands
      ++ ["}"]
  toString (Command name desc) = format "      '{0}[{1}]' \\" [name, desc]

data Dashes = Single | Double deriving (Show, Eq, Ord)
countDashes :: String -> Dashes
countDashes "-" = Single
countDashes "--" = Double

data Argument = Argument {
    dashes :: Dashes
  , flag :: String
  , parameter :: Maybe String
  , description :: String
} deriving (Show, Eq, Ord)
type Arguments = [Argument]

parseArgument :: Parser Arguments
parseArgument = do
  dashes <- parseDashes
  optionalPrefix <- P.optionMaybe $ P.char '[' *> P.many1 (P.noneOf ['[', ']']) <* P.char ']'
  flag <- parseFlag
  parameter <- parseParameter
  tabWidth <- P.lookAhead parseTabWidth
  description <- parseDescription tabWidth
  let argument = Argument dashes flag parameter description
  return $ case optionalPrefix of
    Nothing -> [argument]
    Just prefix -> [argument, withPrefix prefix argument]
  where
    parseTabWidth :: Parser TabWidth
    parseTabWidth = length <$> P.many P.space <?> "tab width"
    parseDashes :: Parser Dashes
    parseDashes = countDashes <$> P.many1 (P.char '-') <?> "dashes"
    parseFlag :: Parser String
    parseFlag = P.many1 nonSpace <* lineSpaces <?> "flag"
    parseParameter :: Parser (Maybe String)
    -- Actually, if a parameter exists then the endOfLine isn't optional, but this is good enough.
    parseParameter = P.optionMaybe parseParameterName <* lineSpaces <* P.optional P.endOfLine <?> "parameter" where
      parseParameterName = P.char '<' *> P.many1 (P.noneOf ['<', '>']) <* P.char '>' <?> "parameter name"
    parseDescription :: TabWidth -> Parser String
    parseDescription tabWidth = intercalate "\n" . formatLines <$> parseLines <?> "description" where
      formatLines = map $ drop tabWidth . trim
      parseLines = P.many1 nonEndLine `P.sepBy1` sep
      sep = P.try $ P.endOfLine <* P.notFollowedBy P.endOfLine
    withPrefix prefix (Argument d f p desc) = Argument d (prefix ++ f) p desc

parseArguments :: Parser Arguments
parseArguments = concat <$> parseArgument `P.sepEndBy1` (P.endOfLine <* P.endOfLine) <?> "arguments"

parseArgumentsSimple :: Parser Arguments
parseArgumentsSimple = do
  unused <- P.skipMany nonFlagLines
  concat <$> parseArgument `P.sepEndBy1` P.try (P.string "--") <?> "simple arguments"
  where
  nonFlagLines = P.try $ lineSpaces <* P.notFollowedBy (P.string "--") <* P.newline

printArguments :: CommandName -> Arguments -> String
printArguments commandName arguments = unlines $ [
      commandName ++ ")", "_arguments \\"] ++
      map toString arguments ++ [";;"] where
  toString (Argument dashes flag parameter description) =
      format "  '{0}[{1}]:{2}:' \\" [printDashes dashes ++ flag, formatDesc description, fromMaybe "" parameter]
  printDashes Single = "-"
  printDashes Double = "--"
  formatDesc :: String -> String
  formatDesc = (=<<) aux where
    aux '\n' = "\n\t\t\t\t"
    aux '\'' = "'\"'\"'"
    aux x = return x

data SimpleArgument = SimpleArgument {
    name :: String
  , description :: [String]
} deriving (Eq, Show)
type SimpleArguments = [SimpleArgument]

parseSimpleArgument :: Parser [SimpleArgument]
parseSimpleArgument = do
  tabWidth <- P.lookAhead parseTabWidth
  optionalPrefix <- lineSpaces *> P.string "--" *> P.optionMaybe (P.string "[no]")
  flag <- nonSpace `P.manyTill` P.char ':' <* lineSpaces <?> "flag"
  description <- parseDescription tabWidth
  return $ case optionalPrefix of
    Nothing -> [SimpleArgument flag description]
    Just _ -> [SimpleArgument flag description, SimpleArgument ("no" ++ flag) description]
    where
    parseDescription :: TabWidth -> Parser [String]
    parseDescription tw = P.many1 nonEndLine `P.sepBy` P.try (P.endOfLine *> P.count tw lineSpace)
    parseTabWidth :: Parser TabWidth
    parseTabWidth = do
      spaces <- P.many lineSpace
      dashes <- P.string "--"
      return $ length $ spaces ++ dashes

parseSimpleArguments :: Parser SimpleArguments
parseSimpleArguments = concat <$> parseSimpleArgument `P.sepEndBy1` P.endOfLine

parseSimpleArgumentsWrapper :: Parser SimpleArguments
parseSimpleArgumentsWrapper = let
    parsePrelude =P.anyChar `P.manyTill` P.try (P.string "flags:\n")
  in parsePrelude *> parseLine *> parseLine *> parseSimpleArguments <* P.endOfLine

printSimpleArguments :: CommandName -> SimpleArguments -> String
printSimpleArguments cn = fmap argsAsCommands .> printCommands cn where
  argsAsCommands :: SimpleArgument -> Command
  -- TODO define an ISO
  argsAsCommands (SimpleArgument n d) = Command n (unlines d)

