{-# LANGUAGE RecordWildCards            #-}

module CLParser where

import Options.Applicative
  ( Parser,
    ParserInfo,
    Mod,
    CommandFields,
    ReadM,
    eitherReader,
    flag,
    fullDesc,
    header,
    option,
    auto,
    long,
    short,
    help,
    argument,
    command,
    helper,
    info,
    metavar,
    progDesc,
    str,
    hsubparser,
    switch,
    subparser,
    prefs,
    showHelpOnEmpty,
    customExecParser
  )

data RunType = Sequential | Parallel

data ScanAlgo = SPS | LDF

data Opts = Opts
     { cmd :: !Command,
       runType :: !RunType,
       noPowerList :: !Bool
     }
-- Add more features here in the future
data Command = Scan ScanAlgo Int 

parser :: Parser Opts
parser = Opts <$> hsubparser scanCommand <*>
  flag Sequential Parallel
    (long "parallel"
    <> short 'p'
    <> help "Run parallel version of the algorithm.")  <*>
  switch
    (long "no-powerlist"
    <> help "Run base algorithm without powerlist")  
  where
      scanCommand :: Mod CommandFields Command
      scanCommand = command "scan" (info scanOptions (progDesc "Run Scan Algorithm"))
      scanOptions :: Parser Command
      scanOptions =
          Scan
          <$> option scanAlgoReader (long "algo" <> short 'a' <> metavar "K" <> help "Supported Algos: SPS, LDF")
          <*> option auto (long "size" <> short 's' <> metavar "R" <> help "Size of array in terms of powers of 2 on which to run scan")
      scanAlgoReader :: ReadM ScanAlgo
      scanAlgoReader = eitherReader $ \arg ->
          case arg of
              "SPS" -> Right(SPS)
              "LDF" -> Right(LDF)
              _ -> Left("Invalid Algo")

parseArgs :: IO Opts
parseArgs = customExecParser (prefs showHelpOnEmpty) $ info
  (helper <*> parser)
  (fullDesc <> progDesc "powerlist" <> header "A program to run algorithms using powerlist abstraction")