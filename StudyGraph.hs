module StudyGraph where

import qualified Data.Set as S
import Data.Time.Calendar (Day)
import Records

type StudyType = (String,Level)

getColumn :: Record -> StudyType
getColumn r = (take 3 $ courseCode r,level r)

getColumns :: [Record] -> S.Set StudyType
getColumns rs = S.fromList $ map getColumn rs

studyRow :: [StudyType] -> Record -> (Day, [Double])
studyRow columns r = (courseDate r,map fillColumn columns)
  where fillColumn c | c == getColumn r = credits r 
                     | otherwise        = 0

studyTable :: [Record] -> [(Day, [Double])]
studyTable rs = 
  where cols = getColumns rs