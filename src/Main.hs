{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ViewPatterns #-}
module Main where


import Data.Monoid

import Data.Time.Calendar


--------------------------------------------------------------------------------

newtype UserId = UserId { unUI :: String }
               deriving (Read,Show,Eq)


data Gender = Male | Female
                     deriving (Read,Show,Eq)

newtype Date = Date { unDay :: Day }
               deriving (Eq,Ord)


instance Show Date where
  show = undefined

instance Read Date where
  readsPrec = undefined


data Person = Person { userId  :: UserId
                     , gender  :: Gender
                     , dueDate :: Date
                     }
              deriving (Read,Show,Eq)


newtype Height = Height { unH :: Int }
                 deriving (Read,Show,Eq,Ord, Num, Integral, Real, Enum)

instance Bounded Height where
  minBound = Height 0
  maxBound = Height 300



data ProgressState = MayStill | HasTo | HasStill | HasOnly | IsLate
                   deriving (Read,Show,Eq,Ord)


--------------------------------------------------------------------------------

main = do print "woei"
