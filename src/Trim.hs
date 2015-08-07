{-# LANGUAGE
    UnicodeSyntax
  , Safe
  #-}

module Trim
  ( trim
  ) where

import Data.Char (isSpace)

trim :: String → String
trim xs = dropSpaceTail "" $ dropWhile isSpace xs

dropSpaceTail :: String → String → String
dropSpaceTail _ "" = ""
dropSpaceTail maybeStuff (x:xs)
        | isSpace x       = dropSpaceTail (x:maybeStuff) xs
        | null maybeStuff = x : dropSpaceTail "" xs
        | otherwise       = reverse maybeStuff ++ x : dropSpaceTail "" xs
