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
            <> help "Name of the Autograder project")
      <*> strArgument
          (metavar "TEMPLATE_PATH"
            <> help "Path to the Canvas gradebook template file")
      <*> switch
          (long "with-test"
            <> short 't'
            <> help "Whether the project has mutation testing")
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
    "Takes a Autograder project name and path to a Canvas gradebook template. \
       \The Autograder export CSV file should be named 'PROJECT.csv'. \
       \Outputs to 'OUTPUT_PATH' if given and stdout otherwise"
    <> Options.Applicative.header
    "Autograter - generate Canvas gradebook from Autograder export")
