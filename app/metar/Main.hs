module Main where

import           Control.Applicative                      ( (<**>)
                                                          , (<|>)
                                                          )
import           Options.Applicative                      ( Parser
                                                          , ParserInfo
                                                          , execParser
                                                          , info
                                                          , helper
                                                          , fullDesc
                                                          , header
                                                          , long
                                                          , help
                                                          , flag'
                                                          , strArgument
                                                          , metavar
                                                          )
import           Data.Attoparsec.ByteString.Char8         ( parseOnly )
import           Data.ByteString                          ( ByteString )
import           METAR.Parsers                            ( metarParser )
import           METAR.Types                              ( prettyMETAR )

data Options
  = METARInput ByteString
  | StreamSTDIN
  deriving (Show)

opts :: ParserInfo Options
opts = info (optsParser <**> helper)
            (fullDesc <> header "METAR records parsing and reporting")

optsParser :: Parser Options
optsParser = metarInput <|> streamStdin
 where
  metarInput  = METARInput <$> strArgument (metavar "METAR record")
  streamStdin = flag' StreamSTDIN (long "stdin" <> help "Read from stdin")

parseAndPrintMETAR :: ByteString -> IO ()
parseAndPrintMETAR input =
  case parseOnly metarParser input of
    Right metar -> putStrLn $ prettyMETAR metar
    Left  e     -> do
      putStrLn "Failed to parse METAR record"
      print e

main :: IO ()
main = do
  o <- execParser opts
  case o of
    METARInput input -> parseAndPrintMETAR input
    StreamSTDIN -> putStrLn "streaming from stdin is not implemented yet"

