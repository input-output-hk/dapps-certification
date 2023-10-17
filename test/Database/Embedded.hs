{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TupleSections   #-}

module Database.Embedded where

import           Data.ByteString            (ByteString)
import           Data.FileEmbed
import           Control.Monad


certificationV0 :: ByteString
certificationV0 =
    $(makeRelativeToProject "test/Data/certification.v0.sqlite" >>= embedFile)

certificationV1 :: ByteString
certificationV1 =
    $(makeRelativeToProject "test/Data/certification.v1.sqlite" >>= embedFile)
