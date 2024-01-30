{-# LANGUAGE OverloadedStrings          #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module IOHK.Certification.Persistence.Structure.Internal where

import           Data.Scientific
import Data.Char as Char
import Data.Text as Text
import Data.Maybe (fromMaybe)
import Data.Fixed (Micro)

dropAndLowerFirst :: Int -> String ->  String
dropAndLowerFirst n = lowerFirst . Prelude.drop n
  where
  lowerFirst :: String -> String
  lowerFirst [] = []
  lowerFirst (x:xs) = Char.toLower x : xs


dropPrefix :: Text -> Text -> Text
dropPrefix prefix text = lowerFirstTChar $
  fromMaybe text  (Text.stripPrefix prefix text)

lowerFirstTChar :: Text -> Text
lowerFirstTChar  text'
  | Text.null text' = text'
  | otherwise = Text.cons (Char.toLower $ Text.head text') (Text.tail text')


{-

Set OverloadedStrings and then:
>>> dropAndLowerFirst 3 "invId"
"id"

>>> :set -XOverloadedStrings
>>> dropPrefix "inv" "invId"
"id"

>>> dropPrefix "inv" "XsX"
"xsX"

-}



scientificToInt64 :: Num a => Scientific -> a
scientificToInt64 = fromInteger . fromInteger . truncate

type DummyAddress = Text
dummyAddress :: DummyAddress
dummyAddress = "addr100000000000000000000000000000000000000000000000000000000"

type AdaUsdPrice = Micro

-- >>> truncate (read "2.2" :: Scientific)
-- >>> scientificToInt64 (read "2.2" :: Scientific)
-- 2
-- 2
