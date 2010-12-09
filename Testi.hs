module Testi where

import Locale (defaultTimeLocale)
import Data.Time.Format (parseTime)
import Data.Time.Calendar (Day,diffDays)
import Text.Parsec.Char
--import Text.Parsec.Text
import Text.Parsec.String
import Text.Parsec.Combinator

title :: GenParser Char () Char
title = do
  string "Jyväskylän yliopisto"
  newline

finnishDate :: GenParser Char () Day
finnishDate = do
  s <- count 10 anyChar
  case fromFinnishDate s of
    Just a -> return a
    Nothing -> fail $ "Expecting date, got '" ++ s ++ "'"
  

-- Some helper functions

-- |Converts Finnish date to Day
fromFinnishDate :: String -> Maybe Day
fromFinnishDate s = parseTime defaultTimeLocale "%d.%m.%Y" s

-- |Converts Day to Unix timestamp. Time of the day will be midnight
-- at UTC.
dayToUnix :: Day -> Integer
dayToUnix day = secondsInADay * (diffDays day unixEpoch)
  where unixEpoch = read "1970-01-01"
        secondsInADay = 24*60*60
        
