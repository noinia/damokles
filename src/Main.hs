{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
module Main where

import Control.Applicative

import Data.Function(on)
import Data.Char(toLower)
import Data.List(sortBy)
import Data.Maybe(fromMaybe)
import Data.Monoid
import Data.String
import Data.Time.Calendar(fromGregorian, diffDays)

import Data.Time.LocalTime

import Text.StringTemplate


import qualified Data.Time.Calendar as C

--------------------------------------------------------------------------------

newtype UserId = UserId { unUI :: String }
               deriving (Read,Show,Eq,IsString)


data Gender = Male | Female
                     deriving (Read,Show,Eq)

newtype Date = Date { unDay :: C.Day }
               deriving (Eq,Ord,Show)

type Year  = Integer
type Month = Int
type Day   = Int

fromDate             :: Year -> Month -> Day -> Date
fromDate yyyy mm dd = Date $ fromGregorian yyyy mm dd


today :: IO Date
today = Date . localDay . zonedTimeToLocalTime <$> getZonedTime



daysUntil                     :: Integral a => Date -> Date -> a
(Date a) `daysUntil` (Date b) = fromInteger $ b `diffDays` a


timeUntil       :: Date -> Date -> (Year, Month, Day)
a `timeUntil` b = collectTime $ a `daysUntil` b
  where
    collectTime t = let years  = t  `div` 365
                        t'     = fromInteger $ t  `mod` 365
                        months = t' `div` 31
                        days   = t' `mod` 31
                    in (years,months,days)
-- TODO: We can make this more precise (e.g. taking care of leap-years etc.) by
-- adding durations/days starting at a, and running until we hit b.




-- instance Show Date where
--   show = showGregorian . unDay

instance Read Date where
  readsPrec = undefined


data Person = Person { userId  :: UserId
                     , gender  :: Gender
                     , dueDate :: Date
                     }
              deriving (Read,Show,Eq)

daysLeft   :: Integral a => Person -> IO a
daysLeft p = (`daysUntil` dueDate p) <$> today



timeLeft   :: Person -> IO (Year,Month,Day)
timeLeft p = (`timeUntil` dueDate p) <$> today



newtype Height = Height { unH :: Int }
                 deriving (Read,Show,Eq,Ord, Num, Integral, Real, Enum)

instance Bounded Height where
  minBound = Height 0
  maxBound = Height 300



data ProgressState = MayStill | HasTo | HasStill | HasOnly | IsLate
                   deriving (Read,Show,Eq,Ord)


toCssClass :: ProgressState -> String
toCssClass = map toLower . show

progressState           :: Year -> ProgressState
progressState y | y < 0 = IsLate
progressState 0         = HasOnly
progressState 1         = HasStill
progressState 2         = HasTo
progressState _         = MayStill


newtype Message = Message { unM :: String }
                  deriving (Show,Eq,Ord,Read,IsString)


message     :: ProgressState -> (Year,Month,Day) -> Message
message s t = Message $ message' s t

message' IsLate   _      = "wordt verondersteld zijn proefschrift te hebben afgerond."
message' HasOnly (0,m,d) = mconcat [ "heeft nog maar "
                                  , show m
                                  , " maanden en "
                                  , show d
                                  , " dagen."
                          ]
message' HasOnly (y,m,d) = mconcat [ "heeft nog maar ", show y, " jaar"
                                  , show m
                                  , " maanden en "
                                  , show d
                                  , " dagen."
                                  ]
message' s (y,m,d) = show (s,y,m,d)




progress   :: Person -> IO (ProgressState,Message)
progress p = (\t@(y,_,_) -> let s = progressState y in (s, message s t))
             <$> timeLeft p


personData   :: Person -> IO (Person,ProgressState,Message)
personData p = (\(s,m) -> (p,s,m)) <$> progress p

--------------------------------------------------------------------------------


users = [ Person "staals" Male $ fromDate 2015 08 31
        , Person "bash"   Male $ fromDate 2012 08 31
        ]



templateDir = "templates/"
templateExt = ".html"

outputFile = "html/generated.html"


loadTemplates :: IO (STGroup String)
loadTemplates = directoryGroupExt templateExt templateDir


itemAttrs                        :: (Person,ProgressState,Message) -> [(String,String)]
itemAttrs ((Person u g d), p, m) = [ ("progressState", toCssClass p)
                                   , ("height", show 100)
                                   , ("picture", unUI u)
                                   , ("homepage", unUI u)
                                   , ("name", unUI u)
                                   , ("message", unM m)
                                   ]


sortOn   :: Ord b => (a -> b) -> [a] -> [a]
sortOn k = sortBy (compare `on` k)

mkHtml                    :: [(Person,ProgressState,Message)] -> STGroup String -> Maybe String
mkHtml userData templates = render' <$> getStringTemplate "item"     templates
                                    <*> getStringTemplate "damokles" templates
  where
    render' itemT = let itemsHtml = [ render $ setManyAttrib (itemAttrs ud) itemT
                                    | ud <- userData
                                    ]
                    in render . setManyAttrib [("items", itemsHtml)]



main = do mHtml <- mkHtml <$> mapM personData (sortOn dueDate users)
                          <*> loadTemplates
          writeFile outputFile $ fromMaybe mempty mHtml
