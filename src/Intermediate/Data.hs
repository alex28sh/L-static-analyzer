{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE FlexibleInstances #-}
module Intermediate.Data where

import           Data.Void       (Void)
import           Text.Megaparsec (Parsec)

type Parser = Parsec Void String

reserved :: [String]
reserved = ["def", "return", "while", "do", "if", "then", "else", "read", "write"]

