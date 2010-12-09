-- |Some helper functions for calendar manipulation

module CalendarHelpers where

import Locale (defaultTimeLocale)
import Data.Time.Calendar (Day,diffDays)
import Data.Time.Format (parseTime)

-- |Converts Finnish date to Day
fromFinnishDate :: String -> Maybe Day
fromFinnishDate s = parseTime defaultTimeLocale "%d.%m.%Y" s

-- |Converts Day to Unix timestamp. Time of the day will be midnight
-- at UTC.
dayToUnix :: Day -> Integer
dayToUnix day = secondsInADay * (diffDays day unixEpoch)
  where unixEpoch = read "1970-01-01"
        secondsInADay = 24*60*60
        
