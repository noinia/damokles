{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
module Main where

import Control.Applicative

import Data.Monoid
import Data.String
import Data.Time.Calendar(fromGregorian, diffDays)

import Data.Time.LocalTime


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

(Date a) `timeUntil` (Date b) = collectTime $ b `diffDays` a
  where
    collectTime t = let years  =               t  `div` 365
                        t'     =               t  `mod` 365
                        months = fromInteger $ t' `div` 31
                        days   = fromInteger $ t' `mod` 31
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

timeLeft   :: Person -> IO (Year,Month,Day)
timeLeft p = (`timeUntil` dueDate p) <$> today




newtype Height = Height { unH :: Int }
                 deriving (Read,Show,Eq,Ord, Num, Integral, Real, Enum)

instance Bounded Height where
  minBound = Height 0
  maxBound = Height 300



data ProgressState = MayStill | HasTo | HasStill | HasOnly | IsLate
                   deriving (Read,Show,Eq,Ord)


--------------------------------------------------------------------------------


users = [ Person "Staals" Male $ fromDate 2015 08 31
        ]

main = do print "woei"
