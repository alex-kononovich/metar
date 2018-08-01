module Main where

import           Control.Monad.IO.Class                   ( MonadIO
                                                          , liftIO
                                                          )
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
                                                          , short
                                                          , help
                                                          , flag'
                                                          , option
                                                          , strArgument
                                                          , str
                                                          , metavar
                                                          )
import           Data.Attoparsec.ByteString.Char8         ( parseOnly )
import           Data.ByteString                          ( ByteString )
import           Conduit                                  ( (.|)
                                                          , runConduitRes
                                                          )
import qualified Data.Conduit.Combinators      as CC
import qualified Data.Map.Strict               as M
import           METAR.Parsers                            ( metarParser )
import           METAR.Types                              ( METAR
                                                          , Report
                                                          , prettyMETAR
                                                          , addMETAR
                                                          , prettyReport
                                                          )

data Input
  = Arg ByteString
  | Stdin
  | File String
  deriving (Show)

opts :: ParserInfo Input
opts = info (inputParser <**> helper)
            (fullDesc <> header "METAR records parsing and reporting")

inputParser :: Parser Input
inputParser = arg <|> standardInput <|> fileInput
 where
  arg  = Arg <$> strArgument (metavar "METAR")
  standardInput = flag' Stdin (long "stdin" <> help "Read from stdin")
  fileInput = File <$> option str (short 'f' <> help "Read from this file")

-- fail early if there is parsing error
parseMETAR :: Monad m => ByteString -> m METAR
parseMETAR input =
  case parseOnly metarParser input of
    Right metar -> return metar
    Left e      -> fail $ unlines
      ["Failed to parse METAR record:"
      , show input
      , show e
      ]

processMETAR :: MonadIO m => Report -> METAR -> m Report
processMETAR report metar = do
  let newReport = addMETAR report metar
  liftIO $ printReport report newReport
  return newReport

printMETAR :: METAR -> IO ()
printMETAR = putStrLn . prettyMETAR

printReport :: Report -> Report -> IO ()
printReport old new = do
  clearPreviousReport old
  putStr $ prettyReport new

clearPreviousReport :: Report -> IO ()
clearPreviousReport report =
  if reportSize == 0
    then return ()
    else do
      cursorUpLine $ 1 + reportSize -- 1 for "Total" line
      clearFromCursorToScreenEnd
  where
    reportSize = M.size report

main :: IO ()
main = do
  input <- customExecParser (prefs showHelpOnError) opts
  case input of
    Arg i  -> parseMETAR i >>= printMETAR
    File f -> processStream (CC.sourceFile f)
    Stdin  -> processStream CC.stdin
  where
    processStream source  = do
      report <- runConduitRes
         $ source
        .| CC.linesUnboundedAscii
        .| CC.mapM (liftIO . parseMETAR)
        .| CC.foldM processMETAR M.empty
      printReport report report
