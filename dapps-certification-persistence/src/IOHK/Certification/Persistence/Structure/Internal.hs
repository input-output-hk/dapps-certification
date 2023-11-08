{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module IOHK.Certification.Persistence.Structure.Internal where

import           Data.Scientific
import Data.Char as Char

dropAndLowerFirst :: Int -> String ->  String
dropAndLowerFirst n = toLowerFirst . drop n
  where
  toLowerFirst :: String -> String
  toLowerFirst [] = []
  toLowerFirst (x:xs) = Char.toLower x : xs



scientificToInt64 :: Num a => Scientific -> a
scientificToInt64 = fromInteger . fromInteger . truncate

-- >>> truncate (read "2.2" :: Scientific)
-- >>> scientificToInt64 (read "2.2" :: Scientific)
-- 2
-- 2
