module METAR.Types
  ( METAR(METAR)
  , mkMetar
  , prettyMETAR
  , formatMETAR
  , ICAOCode
  , mkICAOCode
  , Timestamp
  , mkTimestamp
  , WindInfo
  , mkWindInfo
  , WindSpeed
  , WindSpeedUnit(KT, MPS)
  , Report
  , prettyReport
  , addMETAR
  )
where

import           Data.Maybe                               ( catMaybes )
import           Data.List                                ( intercalate )
import           Data.Word                                ( Word )
import           Data.Char                                ( isUpper )
import           Text.Printf                              ( printf )
import           Data.Semigroup                           ( Semigroup )
import qualified Data.Map.Strict               as M
import qualified Test.QuickCheck               as QC

data METAR = METAR ICAOCode Timestamp WindInfo deriving (Eq, Show)

newtype ICAOCode = ICAOCode String deriving (Eq, Show, Ord)

data Timestamp = Timestamp Day Hours Minutes deriving (Eq, Show)
newtype Day = Day Word deriving (Eq, Show)
newtype Hours = Hours Word deriving (Eq, Show)
newtype Minutes = Minutes Word deriving (Eq, Show)

data WindInfo = WindInfo WindDirection WindSpeed (Maybe WindGusts) deriving (Eq, Show)
newtype WindDirection = WindDirection Word deriving (Eq, Show)
newtype WindSpeed = WindSpeed Word deriving (Eq, Show)
newtype WindGusts = WindGusts Word deriving (Eq, Show)
data WindSpeedUnit = KT | MPS

type Report = M.Map ICAOCode LocationReport

data LocationReport = LocationReport
  { numRecords   :: !Word
  , lastSpeed    :: !Word
  , averageSpeed :: !Double
  } deriving (Show)

--
-- Smart constructors
--

dayRange :: (Word, Word)
dayRange = (1, 31)

hoursRange :: (Word, Word)
hoursRange = (0, 23)

minutesRange :: (Word, Word)
minutesRange = (0, 59)

mkICAOCode :: String -> Either String ICAOCode
mkICAOCode code
  | null code              = Left "ICAO code should contain at least 1 letter"
  | not (all isUpper code) = Left "ICAO code can contain only uppercase letters"
  | otherwise              = Right $ ICAOCode code

mkTimestamp :: Word -> Word -> Word -> Either String Timestamp
mkTimestamp d h m = Timestamp <$> day <*> hours <*> minutes
 where
  day     = Day <$> checkRange "day" dayRange d
  hours   = Hours <$> checkRange "hours" hoursRange h
  minutes = Minutes <$> checkRange "minutes" minutesRange m
  checkRange label (lower, upper) x =
    if x >= lower && x <= upper
      then Right x
      else Left errorText
   where
    errorText =
      label ++ " should be within range of " ++ show lower ++ "-" ++ show upper

mkWindInfo :: Word -> Word -> Maybe Word -> WindInfo
mkWindInfo d s g = WindInfo (WindDirection d) (WindSpeed s) (WindGusts <$> g)

mkMetar
  :: Either String ICAOCode
  -> Either String Timestamp
  -> WindInfo
  -> Either String METAR
mkMetar icaoCode timestamp windInfo =
  METAR <$> icaoCode <*> timestamp <*> Right windInfo

--
-- Reporting
--

instance Semigroup LocationReport where
  r1 <> r2 = LocationReport
    { numRecords   = numRecords r1 + numRecords r2
    , lastSpeed    = lastSpeed r1
    , averageSpeed = (ar1 * nr1 + ar2 * nr2) / (nr1 + nr2)
    }
    where
      nr1 = fromIntegral $ numRecords r1
      nr2 = fromIntegral $ numRecords r2
      ar1 = averageSpeed r1
      ar2 = averageSpeed r2

addMETAR :: METAR -> Report -> Report
addMETAR (METAR icao _ (WindInfo _ (WindSpeed speed) _)) =
  M.insertWith (<>) icao
    LocationReport {numRecords = 1, lastSpeed = speed, averageSpeed = fromIntegral speed}

totalRecords :: Report -> Word
totalRecords = M.foldl' add 0
  where
    add acc r = acc + numRecords r

--
-- Pretty-printing
--

prettyReport :: Report -> String
prettyReport r = unlines $ (prettyRecord <$> M.toList r) ++ [ total ]
  where
    prettyRecord (ICAOCode icao, locationReport) =
      printf "% 5s: average %03.0f m/s, last %03d m/s" icao avgSpeed speed
      where
        speed = lastSpeed locationReport
        avgSpeed = averageSpeed locationReport
    total = printf "Total: %d" $ totalRecords r

prettyMETAR :: METAR -> String
prettyMETAR (METAR (ICAOCode i) t w) =
  i ++ ": " ++ prettyTimestamp t ++ ", " ++ prettyWindInfo w
 where
  prettyTimestamp (Timestamp (Day d) (Hours h) (Minutes m)) =
    printf "day %02d, time %02d:%02d" d h m
  prettyWindInfo (WindInfo d s g) = (intercalate ", " . catMaybes)
    [ Just $ prettyWindDirection d
    , Just $ prettyWindSpeed s
    , prettyWindGusts <$> g
    ]
  prettyWindDirection (WindDirection d) = printf "wind from %d°" d
  prettyWindSpeed (WindSpeed s)         = printf "speed %d m/s" s
  prettyWindGusts (WindGusts g)         = printf "gusts up to %d m/s" g

formatMETAR :: METAR -> String
formatMETAR (METAR (ICAOCode i) (Timestamp (Day d) (Hours h) (Minutes m)) (WindInfo (WindDirection dir) (WindSpeed s) g))
  = printf "%s %02d%02d%02dZ %03d%02d%sMPS" i d h m dir s (formatWindGusts g)
 where
  formatWindGusts Nothing                  = ""
  formatWindGusts (Just (WindGusts gusts)) = printf "G%02d" gusts

--
-- QuickCheck instances
--

instance QC.Arbitrary METAR where
  arbitrary = METAR <$> QC.arbitrary <*> QC.arbitrary <*> QC.arbitrary

instance QC.Arbitrary ICAOCode where
  arbitrary = ICAOCode <$> QC.elements
    ["KATL", "ZBAA", "OMDB", "RJTT", "KLAX", "KORD", "EGLL", "VHHH", "ZSPD", "LFPG"]

instance QC.Arbitrary Timestamp where
  arbitrary = Timestamp <$> QC.arbitrary <*> QC.arbitrary <*> QC.arbitrary

instance QC.Arbitrary Day where
  arbitrary = Day <$> QC.choose dayRange

instance QC.Arbitrary Hours where
  arbitrary = Hours <$> QC.choose hoursRange

instance QC.Arbitrary Minutes where
  arbitrary = Minutes <$> QC.choose minutesRange

instance QC.Arbitrary WindInfo where
  arbitrary = WindInfo <$> QC.arbitrary <*> QC.arbitrary <*> QC.arbitrary

instance QC.Arbitrary WindDirection where
  arbitrary = WindDirection <$> QC.choose (0, 360)

instance QC.Arbitrary WindSpeed where
  arbitrary = WindSpeed <$> QC.choose (0, 50)

instance QC.Arbitrary WindGusts where
  arbitrary = WindGusts <$> QC.choose (0, 99)

