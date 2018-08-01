module Main where

import           System.Console.ANSI                      ( clearFromCursorToScreenEnd
                                                          , cursorUpLine
                                                          )
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
                                                          , long
                                                          , help
                                                          , flag'
                                                          , strArgument
                                                          , metavar
                                                          )
import           Data.Attoparsec.ByteString.Char8         ( parseOnly )
import           Data.ByteString                          ( ByteString )
import qualified Data.ByteString.Char8         as C8
import           Conduit                                  ( (.|)
                                                          , runConduit
                                                          , mapC
                                                          )
import           Data.Conduit.Combinators                 ( stdin
                                                          , mapME
                                                          , foldME
                                                          )
import qualified Data.Map.Strict               as M
import           METAR.Parsers                            ( metarParser )
import           METAR.Types                              ( METAR
                                                          , Report
                                                          , prettyMETAR
                                                          , addMETAR
                                                          , prettyReport
                                                          )

data Mode
  = METARInput ByteString
  | StreamSTDIN
  deriving (Show)

opts :: ParserInfo Mode
opts = info (modeParser <**> helper)
            (fullDesc <> header "METAR records parsing and reporting")

modeParser :: Parser Mode
modeParser = metarInput <|> streamStdin
 where
  metarInput  = METARInput <$> strArgument (metavar "METAR")
  streamStdin = flag' StreamSTDIN (long "stdin" <> help "Read from stdin")

-- fail early if there is parsing error
parseMETAR :: ByteString -> IO METAR
parseMETAR input =
  case parseOnly metarParser input of
    Right metar -> return metar
    Left e      -> fail $ unlines
      ["Failed to parse METAR record:"
      , show input
      , show e
      ]

processMETAR :: Report -> METAR -> IO Report
processMETAR report metar = do
  let newReport = addMETAR report metar

  clearPreviousReport report
  printReport newReport

  return newReport

clearPreviousReport :: Report -> IO ()
clearPreviousReport report =
  if reportSize == 0
    then return ()
    else do
      cursorUpLine $ 1 + reportSize -- 1 for "Total" line
      clearFromCursorToScreenEnd
  where
    reportSize = M.size report

printMETAR :: METAR -> IO ()
printMETAR = putStrLn . prettyMETAR

printReport :: Report -> IO ()
printReport = putStr . prettyReport

main :: IO ()
main = do
  mode <- customExecParser (prefs showHelpOnError) opts
  case mode of
    METARInput input -> parseMETAR input >>= printMETAR
    StreamSTDIN      -> do
      report <- runConduit
         $ stdin
        .| mapC C8.lines
        .| mapME parseMETAR
        .| foldME processMETAR M.empty
      clearPreviousReport report
      printReport report
