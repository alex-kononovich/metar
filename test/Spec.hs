{-# LANGUAGE OverloadedStrings #-}

import           Test.Tasty                               ( defaultMain )
import           Test.Tasty.Hspec                         ( Spec
                                                          , Expectation
                                                          , testSpec
                                                          , describe
                                                          , it
                                                          , shouldBe
                                                          , expectationFailure
                                                          )

import           Data.Attoparsec.ByteString.Char8         ( Parser
                                                          , parseOnly
                                                          )

import           Data.ByteString                          ( ByteString )


import           METAR.Types                              ( mkMetar
                                                          , mkICAOCode
                                                          , mkTimestamp
                                                          , mkWindInfo
                                                          )
import           METAR.Parsers                            ( metarParser
                                                          , icaoCodeParser
                                                          , timestampParser
                                                          , windInfoParser
                                                          )

main :: IO ()
main = defaultMain =<< testSpec "METAR" specs

specs :: Spec
specs =
  describe "parsing" $ do
    describe "ICAO code" $ do
      it "parses valid code" $ do
        icaoCodeParser `shouldParseE` "YYZ" `to` mkICAOCode "YYZ"
        icaoCodeParser `shouldParseE` "A" `to` mkICAOCode "A"
      it "requires at least one character" $
        icaoCodeParser `shouldNotParse` ""
      it "parses only ASCII letters" $
        icaoCodeParser `shouldNotParse` "A23"
      it "parses only uppercase letters" $
        icaoCodeParser `shouldNotParse` "AbbA"
    describe "timestamp" $ do
      it "parses correct timestamps" $ do
        timestampParser `shouldParseE` "122201Z" `to` mkTimestamp 12 22 01
        timestampParser `shouldParseE` "022355Z" `to` mkTimestamp 02 23 55
        timestampParser `shouldParseE` "110232Z" `to` mkTimestamp 11 02 32
      it "requires timestamp to end with Z" $
        timestampParser `shouldNotParse` "110232"
      describe "range" $ do
        it "checks day range 1-31" $
          timestampParser `shouldNotParse` "320232Z"
        it "checks hours range 0-23" $
          timestampParser `shouldNotParse` "112432Z"
        it "checks minutes range 0-59" $
          timestampParser `shouldNotParse` "110260Z"
    describe "wind info" $ do
      it "parses correct wind info" $ do
        windInfoParser `shouldParse` "180120MPS" `to` mkWindInfo 180 120 Nothing
        windInfoParser `shouldParse` "01323G30MPS" `to` mkWindInfo 13 23 (Just 30)
      it "converts knots to meters per second" $
        windInfoParser `shouldParse` "18027KT" `to` mkWindInfo 180 13 Nothing
    describe "METAR" $
      it "parses correct format" $ do
        metarParser `shouldParseE` "YYZ 122201Z 12023MPS" `to`
          mkMetar (mkICAOCode "YYZ") (mkTimestamp 12 22 01) (mkWindInfo 120 23 Nothing)
        metarParser `shouldParseE` "LAX 022355Z 09332G78KT" `to`
          mkMetar (mkICAOCode "LAX") (mkTimestamp 02 23 55) (mkWindInfo 093 16 $ Just 39)
        metarParser `shouldParseE` "FR 110232Z 001100G12MPS" `to`
          mkMetar (mkICAOCode "FR") (mkTimestamp 11 02 32) (mkWindInfo 001 100 $ Just 12)

to :: (Show a, Eq a) => ((a -> Expectation) -> Expectation) -> a -> Expectation
to f expected = f $ shouldBe expected

shouldParseE :: Parser a -> ByteString -> (Either e a -> Expectation) -> Expectation
shouldParseE parser = shouldParse (Right <$> parser)

shouldParse :: Parser a -> ByteString -> (a -> Expectation) -> Expectation
shouldParse parser input expectation = case parseOnly parser input of
  Right actual -> expectation actual
  Left  e      -> expectationFailure $ show e

shouldNotParse :: (Show a) => Parser a -> ByteString -> Expectation
shouldNotParse parser input =
  case parseOnly parser input of
    Right result -> expectationFailure $
      "Expected '" ++ show input ++ "' to fail parsing, but got " ++ show result
    Left _       -> expectationSuccess
  where expectationSuccess = True `shouldBe` True
