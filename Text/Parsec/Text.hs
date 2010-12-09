-- A derivate work from
-- http://stackoverflow.com/questions/4064532/using-parsec-with-data-text

{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Text.Parsec.Text
    ( Parser, GenParser, parseFromUtf8File
    ) where

import Text.Parsec.Prim
import qualified Data.Text as T
import qualified Data.ByteString as B
import Data.Text.Encoding
import Text.Parsec.Error

instance (Monad m) => Stream T.Text m Char where
    uncons = return . T.uncons

type Parser = Parsec T.Text ()
type GenParser t st = Parsec T.Text st

-- | @parseFromUtf8File p filePath@ runs a strict bytestring parser
-- @p@ on the input read from @filePath@ using
-- 'ByteString.readFile'. Returns either a 'ParseError' ('Left') or a
-- value of type @a@ ('Right').
--
-- >  main    = do{ result <- parseFromFile numbers "digits.txt"
-- >              ; case result of
-- >                  Left err  -> print err
-- >                  Right xs  -> print (sum xs)
-- >              }
parseFromUtf8File :: Parser a -> String -> IO (Either ParseError a)
parseFromUtf8File p fname = do 
  raw <- B.readFile fname
  let input = decodeUtf8 raw
  return (runP p () fname input)
