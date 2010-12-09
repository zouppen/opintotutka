module Testi where

import Data.Time.Calendar (Day)
import Text.Parsec.Char
import Text.Parsec.Text
--import Text.Parsec.String
import Text.Parsec.Prim
import Text.Parsec.Combinator
import Control.Monad (liftM)
import CalendarHelpers

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
                
parseJoreFromFile :: String -> IO Student
parseJoreFromFile f = do 
  result <- parseFromUtf8File parseJore f
  case result of
    Left a -> fail $ show a
    Right a -> return a

parseJore :: Parser Student
parseJore = do
  title
  finnishDate
  newline
  newline
  string "                        O P I N T O S U O R I T U S O T E"
  newline
  newline
  student <- studentRecord
  dashline
  return student

studentRecord :: Parser Student
studentRecord = do
  personField "Sukunimi"
  lastName <- stringField
  personField "Etunimet"
  firstName <- stringField
  personField "Syntymäaika"
  space
  birthday <- finnishDate
  newline
  personField "Opinto-oikeus"
  rightToStudy <- stringField
  personField "Tutkintotavoite"
  degreeTarget <- stringField
  personField "Koulutusohjelma"
  degreeProgramme <- maybeStringField
  personField "Erikoiskoulutus"
  alternative <- maybeStringField
  personField "Pääaine"
  majorSubject <- stringField
  personField "Yliopistoon tulovuosi"
  enrolmentYear <- liftM read stringField
  personField "Tiedekunta ja -vuosi"
  faculty <- stringField
  
  return $ Student
    lastName firstName birthday rightToStudy degreeTarget degreeProgramme 
    alternative majorSubject enrolmentYear faculty
  
title :: Parser ()
title = do
  string "Jyväskylän yliopisto"
  newline
  return ()

finnishDate :: Parser Day
finnishDate = do
  s <- count 10 anyChar
  case fromFinnishDate s of
    Just a -> return a
    Nothing -> fail $ "Expecting date, got '" ++ s ++ "'"
  
-- |Expects a given left side value of a field. 
personField :: String -> Parser ()
personField lhs = do
  string lhs
  spaces
  char ':'
  return ()
  
maybeStringField :: Parser (Maybe String)
maybeStringField = (newline >> return Nothing) <|> liftM Just stringField
  
-- |Reads a string until line feed.
stringField :: Parser String
stringField = do
  space
  manyTill anyChar newline

-- |A line of dashes separates person information from credit records.
dashline :: Parser ()
dashline = do 
  manyTill (char '-') newline
  return ()

