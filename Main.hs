module Main where

import System (getArgs)
import Control.Monad (when)
import RecordParser (parseJoreFromFile)
import StudyGraph (joreToCsv)

main = do
  args <- getArgs
  when (length args /= 1) $ error "Usage: program jore.txt"
  let file = args !! 0
  
  jore <- parseJoreFromFile file
  putStr $ joreToCsv jore
