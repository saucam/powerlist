{-# LANGUAGE RecordWildCards #-}

module CLParser where

import Options.Applicative
  ( CommandFields
  , Mod
  , Parser
  , ReadM
  , auto
  , command
  , customExecParser
  , eitherReader
  , fullDesc
  , header
  , help
  , helper
  , hsubparser
  , info
  , long
  , metavar
  , option
  , prefs
  , progDesc
  , short
  , showHelpOnEmpty
  , value
  )

data RunType
  = Sequential
  | Parallel

data ScanAlgo
  = SPS
  | SPSPL
  | SPSPLPar1
  | SPSPLPar2
  | SPSPLPar3
  | LDF
  | LDFPar
  | SPSUBVecPLPar
  | LDFUBVecPLPar
  | LDFChunkUBVecPLPar
  deriving (Show, Enum)

data SortAlgo
  = DEFAULT
  | BATCHER
  deriving (Show, Enum)

newtype Opts =
  Opts
    { cmd :: Command
    }

-- Add more features here in the future
data Command
  = Scan ScanAlgo Int Int
  | Sort SortAlgo Int Int

parser :: Parser Opts
parser = Opts <$> hsubparser (scanCommand <> sortCommand)
  where
    sortCommand :: Mod CommandFields Command
    sortCommand =
      command "sort" (info sortOptions (progDesc "Run Sort Algorithm"))
    sortOptions :: Parser Command
    sortOptions =
      Sort <$>
      option
        sortAlgoReader
        (long "algo" <>
         short 'a' <>
         metavar "ALGONAME" <> help ("Supported Algos: " ++ show [DEFAULT ..])) <*>
      option
        auto
        (long "size" <>
         short 's' <>
         metavar "INPSIZE" <>
         help "Size of array in terms of powers of 2 on which to run sort") <*>
      option
        auto
        (long "csize" <>
         short 'c' <>
         metavar "CHUNKSIZE" <>
         value 64 <> help "Size of chunks for parallelization")
    sortAlgoReader :: ReadM SortAlgo
    sortAlgoReader =
      eitherReader $ \arg ->
        case arg of
          "DEFAULT" -> Right (DEFAULT)
          "BATCHER" -> Right (BATCHER)
          _ -> Left ("Invalid Algo")
    scanCommand :: Mod CommandFields Command
    scanCommand =
      command "scan" (info scanOptions (progDesc "Run Scan Algorithm"))
    scanOptions :: Parser Command
    scanOptions =
      Scan <$>
      option
        scanAlgoReader
        (long "algo" <>
         short 'a' <>
         metavar "ALGONAME" <> help ("Supported Algos: " ++ show [SPS ..])) <*>
      option
        auto
        (long "size" <>
         short 's' <>
         metavar "INPSIZE" <>
         help "Size of array in terms of powers of 2 on which to run scan") <*>
      option
        auto
        (long "csize" <>
         short 'c' <>
         metavar "CHUNKSIZE" <>
         value 64 <> help "Size of chunks for parallelization")
    scanAlgoReader :: ReadM ScanAlgo
    scanAlgoReader =
      eitherReader $ \arg ->
        case arg of
          "SPS" -> Right (SPS)
          "SPSPL" -> Right (SPSPL)
          "SPSPLPar1" -> Right (SPSPLPar1)
          "SPSPLPar2" -> Right (SPSPLPar2)
          "SPSPLPar3" -> Right (SPSPLPar3)
          "LDF" -> Right (LDF)
          "LDFPar" -> Right (LDFPar)
          "SPSUBVecPLPar" -> Right (SPSUBVecPLPar)
          "LDFUBVecPLPar" -> Right (LDFUBVecPLPar)
          "LDFChunkUBVecPLPar" -> Right (LDFChunkUBVecPLPar)
          _ -> Left ("Invalid Algo")

parseArgs :: IO Opts
parseArgs =
  customExecParser (prefs showHelpOnEmpty) $
  info
    (helper <*> parser)
    (fullDesc <>
     progDesc "powerlist" <>
     header "A program to run algorithms using powerlist abstraction")