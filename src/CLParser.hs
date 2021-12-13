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
    customExecParser,
    value
  )

data RunType = Sequential | Parallel

data ScanAlgo = SPS | SPSPL | SPSPLPar1 | SPSPLPar2 | SPSPLPar3 | LDF | LDFPar | BLELLOCH

newtype Opts = Opts { cmd :: Command }
-- Add more features here in the future
data Command = Scan ScanAlgo Int Int

parser :: Parser Opts
parser = Opts <$> hsubparser scanCommand
  where
      scanCommand :: Mod CommandFields Command
      scanCommand = command "scan" (info scanOptions (progDesc "Run Scan Algorithm"))
      scanOptions :: Parser Command
      scanOptions =
          Scan
          <$> option scanAlgoReader (long "algo" <> short 'a' <> metavar "K" <> help "Supported Algos: SPS, LDF")
          <*> option auto (long "size" <> short 's' <> metavar "R" <> help "Size of array in terms of powers of 2 on which to run scan")
          <*> option auto (long "csize" <> short 'c' <> metavar "CHUNKSIZE" <> value 100 <> help "Size of chunks for parallelization")
      scanAlgoReader :: ReadM ScanAlgo
      scanAlgoReader = eitherReader $ \arg ->
          case arg of
              "SPS" -> Right(SPS)
              "SPSPL" -> Right(SPSPL)
              "BLELLOCH" -> Right(BLELLOCH)
              "SPSPLPar1" -> Right(SPSPLPar1)
              "SPSPLPar2" -> Right(SPSPLPar2)
              "SPSPLPar3" -> Right(SPSPLPar3)
              "LDF" -> Right(LDF)
              "LDFPar" -> Right(LDFPar)
              _ -> Left("Invalid Algo")

parseArgs :: IO Opts
parseArgs = customExecParser (prefs showHelpOnEmpty) $ info
  (helper <*> parser)
  (fullDesc <> progDesc "powerlist" <> header "A program to run algorithms using powerlist abstraction")