-- |Contains definition of JORE records

module Records where

import Data.Time.Calendar (Day)

data Student =  Student { lastName :: String
                        , firstName :: String
                        , birthday :: Day
                        , rightToStudy :: String
                        , degreeTarget :: String
                        , degreeProgramme :: Maybe String
                        , alternative :: Maybe String
                        , majorSubject :: String
                        , enrolmentYear :: Integer
                        , faculty :: String
                        } deriving (Show)

data Record = Record { courseCode :: String
                     , courseName :: String
                     , courseDate :: Day
                     , grade      :: Grade
                     , level      :: Level
                     , credits    :: Double
                     } deriving (Show)

data Grade = NumericGrade Int 
           | VerbalGrade VerbalScale 
           | Pass 
           | ThesisGrade ThesisScale
           deriving (Show)

data VerbalScale = Sufficient 
                 | Satisfactory 
                 | Good 
                 | VeryGood 
                 | Excellent
                 deriving (Show)

data ThesisScale = Approbatur
                 | LubenterApprobatur
                 | NonSineLaudeApprobatur
                 | CumLaudeApprobatur
                 | MagnaCumLaudeApprobatur
                 | EximiaCumLaudeApprobatur
                 | Laudatur
                 deriving (Show)

data Level = GeneralStudies
           | LanguageStudies
           | BasicStudies
           | SubjectStudies
           | AdvancedStudies
           | PostgraduateStudies
           deriving (Show, Eq, Ord)
