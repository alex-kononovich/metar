module Main where

import           Control.Applicative                      ( (<**>)
                                                          , (<|>)
                                                          )
import           Options.Applicative                      ( Parser
                                                          , ParserInfo
                                                          , customExecParser
                                                          , showHelpOnError
                                                          , prefs
                                                          , info
                                                          , helper
                                                          , fullDesc
                                                          , header
                                                          , short
                                                          , long
                                                          , option
                                                          , flag'
                                                          , help
                                                          , auto
                                                          , str
                                                          )
import           Conduit                                  ( (.|)
                                                          , runConduitRes
                                                          , mapC
                                                          , liftIO
                                                          )
import qualified Data.Conduit.Combinators      as CC
import qualified Test.QuickCheck               as QC
import           Data.ByteString.Char8                    ( pack )
import           METAR.Types                              ( METAR
                                                          , formatMETAR
                                                          )

data Options = Options
  { numRecors :: Int
  , output    :: Output
  }

data Output = File String | Stdout

opts :: ParserInfo Options
opts = info (optionsParser <**> helper)
            (fullDesc <> header "Random METAR records generator")

optionsParser :: Parser Options
optionsParser = Options <$> numRecords <*> (fileOutput <|> standardOutput)
 where
  numRecords = option auto
    (short 'n' <> help "Number of records to generate")
  fileOutput = File <$> option str
    (short 'f' <> help "Write generated records to this file")
  standardOutput = flag' Stdout
    (long "stdout" <> help "Write generated records to stdout")

randomMETAR :: IO METAR
randomMETAR = QC.generate QC.arbitrary

main :: IO ()
main = do
  options <- customExecParser (prefs showHelpOnError) opts
  runConduitRes
     $ CC.replicateM (numRecors options) (liftIO randomMETAR)
    .| mapC (pack . formatMETAR)
    .| CC.unlinesAscii
    .| sink options
  where
    sink options =
      case output options of
        File f -> CC.sinkFile f
        Stdout -> CC.stdout
