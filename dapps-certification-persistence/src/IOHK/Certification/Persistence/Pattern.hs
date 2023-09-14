{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE DeriveDataTypeable   #-}


{-# LANGUAGE ScopedTypeVariables       #-}


module IOHK.Certification.Persistence.Pattern
( PatternedText(getPatternedText)
, Twitter
, LinkedIn
, Website
, Email
, ProfileWalletAddress
, Subject
, mkPatternedText
, match
, getPattern
) where

import           Data.Text
import           Control.Lens         hiding (index, (.=))
import           Data.Aeson
import           Data.Proxy
import           Data.Swagger         hiding (Contact)
import           Database.Selda
import           Database.Selda.SqlType
import           Control.Exception ( throw)
import           GHC.TypeLits
import           Data.Data

import qualified Data.Swagger.Lens as SL
import qualified Text.Regex.PCRE as PCRE

--------------------------------------------------------------------------------
-- | PatternedText

newtype PatternedText n p = MkUnsafePatternedText { getPatternedText :: Text }
                          deriving (Eq,Ord,Data)

mkPatternedText :: forall p n. KnownSymbol p => Text -> Either String (PatternedText n p)
mkPatternedText t = if IOHK.Certification.Persistence.Pattern.match (pack labelP) t
  then Right $ MkUnsafePatternedText t
  else Left $ "Invalid value for pattern " ++ labelP
  where
  labelP = symbolVal (Proxy :: Proxy p)

instance ToJSON (PatternedText n p) where
  toJSON (MkUnsafePatternedText t) = String t

instance (KnownSymbol n,KnownSymbol p) => FromJSON (PatternedText n p) where
  parseJSON = withText labelN $ \t -> case mkPatternedText t of
      Right value -> pure value
      Left str -> fail str
    where
    labelN = symbolVal (Proxy :: Proxy n)

instance (KnownSymbol n,KnownSymbol p) => SqlType (PatternedText n p) where
  mkLit (MkUnsafePatternedText t) = LCustom TText (LText t)
  sqlType _ = TText
  fromSql (SqlString t) = case mkPatternedText t of
    Right value -> value
    Left str -> throw $ userError $ "fromSql: " ++ str
  fromSql v = throw $ userError $ "fromSql: expected SqlString, got " ++ show v
  defaultValue = throw $ userError $ labelN ++ " : no default value"
    where
    labelN = symbolVal (Proxy :: Proxy n)

instance (KnownSymbol n,KnownSymbol p) => ToSchema (PatternedText n p) where
  declareNamedSchema _ = do
    return $ NamedSchema (Just labelN) $ mempty
      & type_ ?~ SwaggerString
      & SL.pattern ?~ labelP
    where
    labelP = pack $ symbolVal (Proxy :: Proxy p)
    labelN = pack $ symbolVal (Proxy :: Proxy n)

instance Show (PatternedText n p) where
  show (MkUnsafePatternedText t) = show t

getPattern :: forall n p. (KnownSymbol p) => Proxy (PatternedText n p) -> Text
getPattern _ = pack $ symbolVal (Proxy :: Proxy p)

match :: Text -> Text -> Bool
match p val= unpack val PCRE.=~ unpack p

{-

>>> pattern = "^(https?:\\/\\/)?(www\\.)?[-a-zA-Z0-9@:%._\\+~#=]{1,255}\\.[a-z]{2,6}\\b([-a-zA-Z0-9@:%_\\+.~#()?&\\/\\/=]*)$"

>>> match pattern "http://peY.com"
True

>>> pattern = "^(?=.{1,39}$)[a-zA-Z0-9]+(-[a-zA-Z0-9]+)*$"
>>> match pattern "peY-asdad"
True
>>> match pattern "peY-asdad-"
False

-}
--------------------------------------------------------------------------------
-- | Useful aliases


type Twitter = PatternedText "Twitter"
  "^[A-Za-z0-9_]{1,15}$"

type LinkedIn = PatternedText "LinkedIn"
  "^(http(s)?:\\/\\/)?([\\w]+\\.)?linkedin\\.com\\/(pub|in|profile|company)\\/([a-zA-Z0-9_-]+)$"

type Email = PatternedText "Email" "^[A-Z0-9a-z._%+-]+@[A-Za-z0-9.-_]+\\.[A-Za-z]{2,64}$"

type Website = PatternedText "Website"
  "^(https?:\\/\\/)?(www\\.)?[-a-zA-Z0-9@:%._\\+~#=]{1,255}\\.[a-z]{2,6}(\\b([-a-zA-Z0-9@:%_\\+.~#()?&\\/\\/=]*))?$"

type ProfileWalletAddress = PatternedText "ProfileWalletAddress"
  "^(addr_test1|addr1|stake|stake_test1)[a-zA-Z0-9]{53,}$"

type Subject = PatternedText "Subject" "^[A-Za-z0-9_]{1,64}$"
