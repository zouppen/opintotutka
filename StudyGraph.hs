module StudyGraph where

import Data.List (intercalate)
import qualified Data.Set as S
import qualified Data.Map as M
import Data.Time.Calendar (Day)
import Records
import CalendarHelpers (dayToUnix)
import RecordParser (getLevelLetter)

type StudyType = (String,Level)

-- NB! This uses S.toList which is not ascending. That's used in
-- several places. Rememember to change them everywhere if you need to
-- modify them or otherwise odd behaviour occurs (columns and labels
-- may mismatch then).

-- |Reads a single column into a pair of course type and level.
getColumn :: Record -> StudyType
getColumn r = (take 3 $ courseCode r,level r)

-- |Reads a list of records into set of distinct elements.
getColumns :: [Record] -> S.Set StudyType
getColumns rs = S.fromList $ map getColumn rs

-- |When provided with StudyType list, converts a record into a result
-- row where credits are set to zero on columns which do not match.
studyRow :: [StudyType] -> Record -> (Day, [Double])
studyRow columns r = (courseDate r,map fillColumn columns)
  where fillColumn c | c == getColumn r = credits r 
                     | otherwise        = 0

-- |Converts records into an unorganized table of dates and credits.
studyTable :: [Record] -> [(Day, [Double])]
studyTable rs = map f rs
  where cols = getColumns rs
        f    = studyRow (S.toList cols)

-- |Converts day-columns pairs to cumulative list, summing records on
-- a single day.
cumulativeTable :: [(Day, [Double])] -> [(Day, [Double])]
cumulativeTable list = scanl1 sumPairList distinct
  where distinct' = M.fromListWith (zipWith (+)) list
        distinct  = M.toAscList distinct'
        sumPairList (_,a) (x,b) = (x,zipWith (+) a b)
        
joreToCsv :: (Student, [Record]) -> String
joreToCsv (_,rs) = header ++ "\n" ++ unlines rows
  where header = toCsv $ "timestamp":(map showCourse $ S.toList $ getColumns rs)
        rows   = map rowToText $ cumulativeTable $ studyTable rs
        showCourse (prefix,level) = prefix ++ [getLevelLetter level]

rowToText (day,xs) = toCsv $ (show $ dayToUnix day):(map show xs)
toCsv   = intercalate ","
