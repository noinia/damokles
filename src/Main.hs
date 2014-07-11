{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
module Main where

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
        , Person "bash"   "Bas de Haas"  Male $ fromDate 2012 08 31
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

daysLeft   :: Integral a => Person -> IO a
daysLeft p = (`daysUntil` dueDate p) <$> today



timeLeft   :: Person -> IO (Year,Month,Day)
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
mkMessage s t = Message $ message' s t

message' IsLate   _      = "wordt verondersteld zijn proefschrift te hebben afgerond."
message' HasOnly (0,m,d) = mconcat [ "heeft nog maar "
                                  , showT m
                                  , " maanden en "
                                  , showT d
                                  , " dagen."
                          ]
message' HasOnly (y,m,d) = mconcat [ "heeft nog maar ", showT y, " jaar"
                                  , showT m
                                  , " maanden en "
                                  , showT d
                                  , " dagen."
                                  ]
message' s (y,m,d) = showT (s,y,m,d)


renderData   :: Person -> IO RenderData
renderData p = renderData' <$> timeLeft p
  where
    renderData' t@(y,_,_) = let s = progressState y in
                            RenderData p (calcHeight t) s (mkMessage s t)

calcHeight = const 200

--------------------------------------------------------------------------------
-- | Html templates

loadTemplates :: IO (STGroup Text)
loadTemplates = mergeSTGroups <$> directoryGroupExt htmlTemplateExt    templateDir
                              <*> directoryGroupExt messageTemplateExt templateDir


itemAttrs                                       :: RenderData -> [(String,Text)]
itemAttrs (RenderData p@(Person u n g d) h s m) = [ ("progressState", toCssClass s)
                                                  , ("height", showT . unH $ h)
                                                  , ("picture", unUI u)
                                                  , ("homepage", homepage p)
                                                  , ("name", unN n)
                                                  , ("message", unM m)
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
