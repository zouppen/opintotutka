module ParsingHelpers where

-- |Converts a value (usually String or Char) to another value with
-- the help of a lookup list. If nothing is found, then error message
-- is thrown.
convert :: (Eq a) => [(a, b)] -> String -> a -> b
convert list msg a = case lookup a list of
  Just a -> a
  Nothing -> error msg
