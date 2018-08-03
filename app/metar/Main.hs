{-# LANGUAGE LambdaCase #-}

module Main where

import           Control.Exception                        ( evaluate )
import           Control.Concurrent                       ( MVar
                                                          , newMVar
                                                          , modifyMVar_
                                                          , readMVar
                                                          , forkIO
                                                          , threadDelay
                                                          )
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

data State = Running | StopUI | StopMain

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

processMETAR :: MonadIO m => MVar Report -> ByteString -> m ()
processMETAR mReport s = do
  metar <- parseMETAR s
  liftIO $ modifyMVar_ mReport (evaluate . addMETAR metar)

printMETAR :: METAR -> IO ()
printMETAR = putStrLn . prettyMETAR

printReport :: MVar Report -> Report -> IO Report
printReport mReport oldReport = do
  newReport <- readMVar mReport
  if M.size newReport == 0
    then return newReport
    else do
      clearPreviousReport oldReport
      putStr $ prettyReport newReport
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

updateUI :: MVar State -> MVar Report -> Report -> IO ()
updateUI mState mReport oldReport = do
  report <- printReport mReport oldReport
  threadDelay 300000
  readMVar mState >>= \case
    Running -> updateUI mState mReport report
    StopUI -> do
      _ <- printReport mReport report
      modifyMVar_ mState (return . const StopMain)
    _ -> return ()

waitForExit :: MVar State -> IO ()
waitForExit mState =
  readMVar mState >>= \case
    StopMain -> return ()
    _ -> threadDelay 100000 >> waitForExit mState

main :: IO ()
main =
  customExecParser (prefs showHelpOnError) opts >>= \case
    Arg i  -> parseMETAR i >>= printMETAR
    File f -> processStream (CC.sourceFile f)
    Stdin  -> processStream CC.stdin
  where
    initialReport = M.empty
    processStream source  = do
      mReport <- newMVar initialReport
      mState <- newMVar Running
      _ <- forkIO (updateUI mState mReport initialReport)
      runConduitRes
         $ source
        .| CC.linesUnboundedAscii
        .| CC.mapM_ (processMETAR mReport)
      modifyMVar_ mState (return . const StopUI)
      waitForExit mState
