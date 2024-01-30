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

certificationV2 :: ByteString
certificationV2 =
    $(makeRelativeToProject "test/Data/certification.v2.sqlite" >>= embedFile)

certificationV3 :: ByteString
certificationV3 =
    $(makeRelativeToProject "test/Data/certification.v3.sqlite" >>= embedFile)

certificationV4 :: ByteString
certificationV4 =
    $(makeRelativeToProject "test/Data/certification.v4.sqlite" >>= embedFile)
