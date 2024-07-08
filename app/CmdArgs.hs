module CmdArgs where

import Options.Applicative

data AppOutput = StdOut | OutputFile !FilePath
  deriving (Show)

data CmdArgs = CmdArgs {
  projectName      :: !String,      {- required -}
  templatePath     :: !FilePath,    {- required -}
  withTest         :: !Bool,
  outputPath       :: !AppOutput }
  deriving (Show)

parseArgs :: Options.Applicative.Parser CmdArgs
parseArgs = CmdArgs
      <$> strArgument
          (metavar "PROJECT"
            <> help "Name of your Autograder project")
      <*> strArgument
          (metavar "TEMPLATE_PATH"
            <> help "Path to the Canvas gradebook template file")
      <*> switch
          (long "with-test"
            <> short 't'
            <> help "Whether your project has a mutation testing component")
      <*> option (OutputFile <$> str)
          (long "output"
            <> short 'o'
            <> metavar "OUTPUT_PATH"
            <> help "Path to the output file"
            <> value StdOut)

opts :: ParserInfo CmdArgs
opts = info (parseArgs <**> helper)
  (fullDesc
    <> Options.Applicative.progDesc
    "The program takes an Autograder project name (`PROJECT`) and the path to a        \
    \Canvas gradebook template (`TEMPLATE_PATH`). The program expects the Autograder   \
    \export CSV file to be named `PROJECT.csv`. If there exists a mutation testing     \
    \component for your assignment, then the mutation testing part should live in      \
    \a separate Autograder project and its export should be named `PROJECT_TEST.csv`.  \
    \The program generates a file if `OUTPUT_PATH` is supplied; otherwise, the program \
    \prints to standard output."
    <> Options.Applicative.header
    "Autograter - generates Canvas gradebook import from Autograder export")
