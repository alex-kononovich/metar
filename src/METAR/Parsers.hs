{-# LANGUAGE OverloadedStrings #-}

module METAR.Parsers
  ( metarParser
  , icaoCodeParser
  , timestampParser
  , windInfoParser
  )
where

import           Control.Monad                            ( void )
import           Control.Applicative                      ( (<|>) )
import           Data.Word                                ( Word )
import           Data.List                                ( foldl' )
import           Data.Char                                ( digitToInt )
import           Data.Attoparsec.ByteString.Char8         ( Parser
                                                          , (<?>)
                                                          , many1
                                                          , option
                                                          , satisfy
                                                          , count
                                                          , string
                                                          , char
                                                          , space
                                                          , inClass
                                                          , isDigit
                                                          , endOfLine
                                                          , endOfInput
                                                          )

import           METAR.Types                              ( METAR(METAR)
                                                          , ICAOCode
                                                          , mkICAOCode
                                                          , Timestamp
                                                          , mkTimestamp
                                                          , WindInfo
                                                          , mkWindInfo
                                                          , WindSpeedUnit(KT, MPS)
                                                          )

metarParser :: Parser METAR
metarParser = token metar <?> "METAR record"
  where metar = METAR <$> icaoCodeParser <*> timestampParser <*> windInfoParser

icaoCodeParser :: Parser ICAOCode
icaoCodeParser = token (liftEither icaoCode) <?> "ICAO code"
 where
  uppercaseLetter = satisfy (inClass "A-Z") <?> "uppercase letter"
  icaoCode        = mkICAOCode <$> many1 uppercaseLetter

timestampParser :: Parser Timestamp
timestampParser = token (liftEither timestamp) <?> "timestamp"
 where
  timestamp = mkTimestamp <$> day <*> hours <*> minutes <* zulu
  day       = naturalNumber 2 <?> "day"
  hours     = naturalNumber 2 <?> "hours"
  minutes   = naturalNumber 2 <?> "minutes"
  zulu      = char 'Z'

windInfoParser :: Parser WindInfo
windInfoParser = token $ do
  direction <- naturalNumber 3 <?> "wind direction"
  speed     <- naturalNumber 3 <|> naturalNumber 2 <?> "wind speed"
  gusts     <- optional (char 'G' *> naturalNumber 2) <?> "wind gusts"
  converter <- convertToMPS <$> windSpeedUnitParser
  return $ mkWindInfo direction (converter speed) (converter <$> gusts)
 where
  convertToMPS KT  = flip quot 2
  convertToMPS MPS = id
  optional p = option Nothing (Just <$> p)

windSpeedUnitParser :: Parser WindSpeedUnit
windSpeedUnitParser =
  (const MPS <$> string "MPS") <|> (const KT <$> string "KT")
  <?> "wind speed unit"

--
-- Helpers
--

token :: Parser a -> Parser a
token p = p <* (spaceSeparator <|> endOfLine <|> endOfInput)
  where spaceSeparator = void space

naturalNumber :: Int -> Parser Word
naturalNumber numDigits = foldl' step 0 <$> count numDigits (satisfy isDigit)
  where step a w = a * 10 + fromIntegral (digitToInt w)

liftEither :: Monad m => m (Either String a) -> m a
liftEither p = do
  result <- p
  case result of
    Right x -> return x
    Left e  -> fail e
