{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
module Main where

import Control.Arrow((&&&))
import Control.Applicative

import Data.Function(on)
import Data.List(sortBy)
import Data.Maybe(fromMaybe)
import Data.Monoid
import Data.String(IsString(..))

import Data.Time.Calendar(fromGregorian, diffDays)
import Data.Time.LocalTime

import Data.Text.Lazy(Text)
import Data.Text.Format
import Text.StringTemplate


import qualified Data.Time.Calendar as C
import qualified Data.Text.Lazy     as T
import qualified Data.Text.Lazy.IO  as IO

--------------------------------------------------------------------------------
-- | Settings

users = [ Person "staals" "Frank Staals" Male $ fromDate 2015 08 31
        , Person "bash"   "Bas de Haas"  Female $ fromDate 2012 08 31
        , Person "newguy" "New Guy"      Male $ fromDate 2020 01 01
        ]



templateDir = "templates/"
htmlTemplateExt    = ".html"
messageTemplateExt = ".st"

outputFile = "html/generated.html"

--------------------------------------------------------------------------------

newtype Date = Date { unDay :: C.Day }
               deriving (Eq,Ord,Show)

type Year  = Integer
type Month = Int
type Day   = Int

type TotalDays = Integer

fromDate             :: Year -> Month -> Day -> Date
fromDate yyyy mm dd = Date $ fromGregorian yyyy mm dd


today :: IO Date
today = Date . localDay . zonedTimeToLocalTime <$> getZonedTime



daysUntil                     :: Integral a => Date -> Date -> a
(Date a) `daysUntil` (Date b) = fromInteger $ b `diffDays` a


timeUntil       :: Date -> Date -> ((Year, Month, Day), TotalDays)
a `timeUntil` b = collectTime &&& id $ a `daysUntil` b
  where
    collectTime t = let years  = t  `div` 365
                        t'     = fromInteger $ t  `mod` 365
                        months = t' `div` 31
                        days   = t' `mod` 31
                    in (years,months,days)
-- TODO: We can make this more precise (e.g. taking care of leap-years etc.) by
-- adding durations/days starting at a, and running until we hit b.

instance Read Date where
  readsPrec = undefined -- TODO

--------------------------------------------------------------------------------

newtype UserId = UserId { unUI :: Text }
               deriving (Read,Show,Eq,IsString)

data Gender = Male | Female
                     deriving (Read,Show,Eq)

newtype Name = Name { unN :: Text }
               deriving (Show,Eq,Ord,Read,IsString)


data Person = Person { userId  :: UserId
                     , name    :: Name
                     , gender  :: Gender
                     , dueDate :: Date
                     }
              deriving (Read,Show,Eq)

timeLeft   :: Person -> IO ((Year,Month,Day),TotalDays)
timeLeft p = (`timeUntil` dueDate p) <$> today

homepage   :: Person -> Text
homepage p = let us = unUI . userId $ p in
             format "http://www.cs.uu.nl/staff/{}.html" $ Only us

--------------------------------------------------------------------------------
-- | Buisness logic


data RenderData = RenderData { person   :: Person
                             , height   :: Height
                             , progress :: ProgressState
                             , message  :: Message
                             }
                  deriving (Show,Eq)

newtype Height = Height { unH :: Int }
                 deriving (Read,Show,Eq,Ord, Num, Integral, Real, Enum)

instance Bounded Height where
  minBound = Height 0
  maxBound = Height 300



data ProgressState = MayStill | HasTo | HasStill | HasOnly | IsLate
                   deriving (Read,Show,Eq,Ord)


toCssClass :: ProgressState -> Text
toCssClass = T.toLower . showT

progressState           :: Year -> ProgressState
progressState y | y < 0 = IsLate
progressState 0         = HasOnly
progressState 1         = HasStill
progressState 2         = HasTo
progressState _         = MayStill


newtype Message = Message { unM :: Text }
                  deriving (Show,Eq,Ord,Read,IsString)


mkMessage     :: ProgressState -> (Year,Month,Day) -> Message
mkMessage s t = Message $ message'' NL Male s t


data Language = NL | EN
              deriving (Show,Read,Eq)

day, days, month, months, year, years    :: Language -> Text
day NL = "dag"
day EN = "day"

days NL = "dagen"
days EN = "days"

month NL = "maand"
month EN = "month"

months NL = "maanden"
months EN = "months"

year NL = "jaar"
year EN = "year"

years NL = "jaar"
years EN = "years"


mkDays          :: Language -> Year -> Month -> Day -> Text
mkDays l  0 0 d = mkDays' l d
mkDays NL _ _ d = "en "  <> mkDays' NL d
mkDays EN _ _ d = "and " <> mkDays' EN d

mkDays'     :: Language -> Day -> Text
mkDays' _ 0 = ""
mkDays' l 1 = format "1 {}"  $ Only (day l)
mkDays' l d = format "{} {}"   (d, (days l))

mkMonths     :: Language -> Month -> Text
mkMonths _ 0 = ""
mkMonths l 1 = format "1 {}"  $ Only (month l)
mkMonths l m = format "{} {}"   (m,months l)

mkYears     :: Language -> Year -> Text
mkYears _ 0 = ""
mkYears l 1 = format "1 {}" $ Only (year l)
mkYears l y = format "{} {}"  (y,year l)

hisHer           :: Language -> Gender -> Text
hisHer NL Male   = "zijn"
hisHer NL Female = "haar"
hisHer EN Male   = "his"
hisHer EN Female = "her"


isLate, hasOnly, hasStill, mayStill    :: Language -> Format
isLate NL = "wordt verondersteld {} proefschrift te hebben afgerond."
isLate EN = "is supposed to be done with {} thesis."

hasOnly NL = "heeft nog maar {} {} {}"
hasOnly EN = ""

hasStill NL = "heeft nog {} {} {}"


hasTo NL = "moet nog {} {} {}"

mayStill NL = "mag nog {} {} {}"


message''                      :: Language -> Gender -> ProgressState -> (Year,Month,Day) -> Text
message'' l g IsLate    _      = format (isLate   l) (Only $ hisHer l g)
message'' l _ HasOnly  (y,m,d) = format (hasOnly  l) (mkYears l y, mkMonths l m, mkDays l y m d)
message'' l _ HasStill (y,m,d) = format (hasStill l) (mkYears l y, mkMonths l m, mkDays l y m d)
message'' l _ HasTo    (y,m,d) = format (hasTo    l) (mkYears l y, mkMonths l m, mkDays l y m d)
message'' l _ MayStill (y,m,d) = format (mayStill l) (mkYears l y, mkMonths l m, mkDays l y m d)


renderData   :: Person -> IO RenderData
renderData p = renderData' <$> timeLeft p
  where
    renderData' (tl@(y,_,_),td) = let s = progressState y in
                                  RenderData p (calcHeight td) s (mkMessage s tl)

calcHeight    :: Integer -> Height
calcHeight td = Height $ min' + floor (frac * len)
  where
    max' = unH (maxBound :: Height)
    min' = unH (minBound :: Height)
    len = fromIntegral $ max' - min'

    maxDays = 4*365  -- Default duration of a Phd = 4 years
    frac    :: Double
    frac    = max 0.0 . min 1.0 $ (fromInteger td) / maxDays
              -- Fraction of the (maximum) time remaining

--------------------------------------------------------------------------------
-- | Html templates

loadTemplates :: IO (STGroup Text)
loadTemplates = mergeSTGroups <$> directoryGroupExt htmlTemplateExt    templateDir
                              <*> directoryGroupExt messageTemplateExt templateDir


itemAttrs                                       :: RenderData -> [(String,Text)]
itemAttrs (RenderData p@(Person u n g d) h s m) = [ ("progressState", toCssClass s)
                                                  , ("height",        showT . unH $ h)
                                                  , ("picture",       unUI u)
                                                  , ("homepage",      homepage p)
                                                  , ("name",          unN n)
                                                  , ("message",       unM m)
                                                  ]

--------------------------------------------------------------------------------
-- | The main program

main = do mHtml <- mkHtml <$> mapM renderData (sortOn dueDate users)
                          <*> loadTemplates
          IO.writeFile outputFile $ fromMaybe mempty mHtml

mkHtml                    :: [RenderData] -> STGroup Text -> Maybe Text
mkHtml userData templates = render' <$> getStringTemplate "item"     templates
                                    <*> getStringTemplate "damokles" templates
  where
    render' itemT = let itemsHtml = [ render $ setManyAttrib (itemAttrs ud) itemT
                                    | ud <- userData
                                    ]
                    in render . setManyAttrib [("items", itemsHtml)]

--------------------------------------------------------------------------------
-- | Generic utility functions

showT :: Show a => a -> Text
showT = T.pack . show

sortOn   :: Ord b => (a -> b) -> [a] -> [a]
sortOn k = sortBy (compare `on` k)
