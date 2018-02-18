module Main where

import LibInternal
import Test.HUnit
import qualified Text.Parsec as P
import Common.Operators
import Data.List(intercalate)
import Common.Parsec(Parser)

right :: Show a => Either a b -> b
right (Right x) = x
right (Left x) = error $ "Expected successful but was " ++ show x

successfulParse :: Parser a -> String -> a
successfulParse parser str = P.parse parser "(test string)" str |> right

tests = test [
    "pareCommand simple" ~: do
    let str = "     citc           Creates or deletes a CitC client.                         "
    let command = successfulParse (parseCommand 0) str
    let expected = Command "citc" "Creates or deletes a CitC client."
    expected ~=? command
  , "pareCommand with newLine" ~: do
    let str = "sync           Update local files with changes in the depot or client\n" ++
              "               view."
    let tabWidth = successfulParse parseTabWidth str
    let command = successfulParse (parseCommand tabWidth) str
    let expected = Command "sync" "Update local files with changes in the depot or client view."
    expected ~=? command
  , "pareCommands with two commands and an empty line" ~: do
    let str = "foo bar\nbazz qux"
    [Command "foo" "bar", Command "bazz" "qux"] ~=? successfulParse parseCommands str
  , "pareCommands with two commands and a white-space line" ~: do
    let str = "foo bar\n  \nbazz qux"
    [Command "foo" "bar", Command "bazz" "qux"] ~=? successfulParse parseCommands str
  , "parseCommands complex" ~: do
    let str = unlines [
           "       citc           Creates or deletes a CitC client.   "
         , ""
         , ""
         , "       pending        Display pending changelists.    "
         , "       diff           Compare client file(s) against depot file(s).   "
         , ""
         , "       sync           Update local files with changes in the depot or client\n                      view."
         , ""
         , "       commit         Commit (snapshot) current state of the client."
         , ""
         ]
    let command = successfulParse parseCommands str
    let expected = [
           Command "citc" "Creates or deletes a CitC client."
         , Command "pending" "Display pending changelists."
         , Command "diff" "Compare client file(s) against depot file(s)."
         , Command "sync" "Update local files with changes in the depot or client view."
         , Command "commit" "Commit (snapshot) current state of the client."
         ]
    expected ~=? command

  , "parseArgument single line" ~: do
    let str = "-n  Tests submitting a change without actually submitting it."
    let argument = successfulParse parseArgument str
    argument ~?= [Argument Single "n" Nothing "Tests submitting a change without actually submitting it."]
  , "parseArgument one new line" ~: do
    let str = intercalate "\n" [
           "--ignore_presubmit_warnings"
         , "    Ignores warnings from METADATA Presubmit check execution and continues to submit the changelist."
         ]
    let argument = successfulParse parseArgument str
    argument ~?= [Argument Double "ignore_presubmit_warnings" Nothing
        "Ignores warnings from METADATA Presubmit check execution and continues to submit the changelist."]
  , "parseArgument multiple new line" ~: do
    let str = intercalate "\n" [
           "--auto_cleanup_workspace"
         , "    Deletes the workspace after successful submission if no other changes, open files, or"
         , "    unreconciled files are present. This option is only applicable to CitC clients."
         ]
    let argument = successfulParse parseArgument str
    argument ~?= [Argument Double "auto_cleanup_workspace" Nothing (intercalate "\n" [
          "Deletes the workspace after successful submission if no other changes, open files, or"
        , "unreconciled files are present. This option is only applicable to CitC clients." ])]
  , "parseArgument with parameter" ~: do
    let str = intercalate "\n" [
           "--tap_projects <projects>"
         , "    Specifies a comma-separated list of TAP projects for which tests will be executed. This list is"
         , "    merged with the projects configured by the CheckTests METADATA Presubmit check."
         ]
    let argument = successfulParse parseArgument str
    argument ~?= [Argument Double "tap_projects" (Just "projects") (intercalate "\n" [
          "Specifies a comma-separated list of TAP projects for which tests will be executed. This list is"
        , "merged with the projects configured by the CheckTests METADATA Presubmit check." ])]
  , "parseArgument with nested tabs" ~: do
    let str = intercalate "\n" [
           "-a <action>"
         , "    Specifies the action for which presubmit checks are executed. The default is \"submit\". See the"
         , "    Action enum in \"p4 help metadata\" for more information."
         , "         mail|review        Executes checks configured for \"Action.REVIEW\"."
         , "         submit             Executes checks configured for \"Action.SUBMIT\"."
          ]
    let argument = successfulParse parseArgument str
    argument ~?= [Argument Single "a" (Just "action") (intercalate "\n" [
           "Specifies the action for which presubmit checks are executed. The default is \"submit\". See the"
         , "Action enum in \"p4 help metadata\" for more information."
         , "     mail|review        Executes checks configured for \"Action.REVIEW\"."
         , "     submit             Executes checks configured for \"Action.SUBMIT\"." ])]
  , "parseArgument with optional prefix" ~: do
    let str = "--[no]foo bar"
    let expected = [Argument Double "foo" Nothing "bar", Argument Double "nofoo" Nothing "bar"]
    expected ~=? successfulParse parseArgument str
  , "parseArguments" ~: do
    let str = intercalate "\n" [
           "-c <change#>"
         , "   Specifies the changelist for which checks are executed."
         , ""
         , "-n  Outputs the checks that are configured for the changelist without actually executing them."
         , ""
         , "--filter <filter>"
         , "   A regular expression that filters which presubmits to run, by name. The expression is not"
         , "   anchored by default. The presubmit's name can be seen in output. For example, \"--filter Tests\""
         , "   would run \"CheckTests/0\" and not \"CheckCopyright\"." ]
    let actual = successfulParse parseArguments str
    actual ~?= [
        Argument Single "c" (Just "change#") "Specifies the changelist for which checks are executed."
      , Argument Single "n" Nothing "Outputs the checks that are configured for the changelist without actually executing them."
      , Argument Double "filter" (Just "filter") (intercalate "\n" [
           "A regular expression that filters which presubmits to run, by name. The expression is not"
         , "anchored by default. The presubmit's name can be seen in output. For example, \"--filter Tests\""
         , "would run \"CheckTests/0\" and not \"CheckCopyright\"." ]) ]

    -- simple args
  , "parseSimpleArgument" ~: do
    successfulParse parseSimpleArgument "--[no]foo: bar bazz" ~?= [
        SimpleArgument "foo" ["bar bazz"]
      , SimpleArgument "nofoo" ["bar bazz"] ]
  , "parseSimpleArgument multi line" ~: do
    let str = "  --additional_task_memory: Additional memory for each GWS task beyond the\n" ++
              "    default used for the production config."
    successfulParse parseSimpleArgument str ~?= [
        SimpleArgument "additional_task_memory" ["Additional memory for each GWS task beyond the",
            "default used for the production config."] ]
  , "parseSimpleArgument multi line with -- as part of description" ~: do
    let str = "  --additional_task_memory: Additional memory for each GWS task beyond the\n" ++
              "    --default used for the production config."
    successfulParse parseSimpleArgument str ~?= [
        SimpleArgument "additional_task_memory" ["Additional memory for each GWS task beyond the",
            "--default used for the production config."] ]
  , "parseSimpleArguments" ~: do
    let str = intercalate "\n" [
            "  --addargs: Additional command-line arguments for gws. This list of string is"
          , "    appended to the gws command line, so the format of this argument is:"
          , "    --addargs=--flag1=value1 --addargs=--flag2=value2 --addargs=\"--flag3=value3"
          , "    --flag4=value4 ...\";"
          , "    repeat this option to specify a list of values"
          , "    (default: '[]')"
          , "  --additional_task_memory: Additional memory for each GWS task beyond the"
          , "    default used for the production config."
          , "  --[no]also_launch_esf_proxy: If True, start a Mothership ESF proxy server that"
          , "    can forward external One Platform requests to Mothership."
          , "    (default: 'false')" ]
    successfulParse parseSimpleArguments str ~?= [
        SimpleArgument "addargs" [
              "Additional command-line arguments for gws. This list of string is"
            , "appended to the gws command line, so the format of this argument is:"
            , "--addargs=--flag1=value1 --addargs=--flag2=value2 --addargs=\"--flag3=value3"
            , "--flag4=value4 ...\";"
            , "repeat this option to specify a list of values"
            , "(default: '[]')" ]
      , SimpleArgument "additional_task_memory" ["Additional memory for each GWS task beyond the",
            "default used for the production config."]
      , SimpleArgument "also_launch_esf_proxy" [
            "If True, start a Mothership ESF proxy server that"
          , "can forward external One Platform requests to Mothership."
          , "(default: 'false')" ]
      , SimpleArgument "noalso_launch_esf_proxy" [
            "If True, start a Mothership ESF proxy server that"
          , "can forward external One Platform requests to Mothership."
          , "(default: 'false')" ]
      ]
  , "parseSimpleArgumentsWrapper" ~: do
    let str = intercalate "\n" [
            "yada yada yada"
          , "bla bla bla"
          , "flags:"
          , ""
          , "foo_flags:"
          , "  --foo: bar bazz"
          , ""
          , "ta tada tada"
          ]
    successfulParse parseSimpleArgumentsWrapper str ~?= [SimpleArgument "foo" ["bar bazz"]]
  ]

main :: IO Counts
main = runTestTT tests


