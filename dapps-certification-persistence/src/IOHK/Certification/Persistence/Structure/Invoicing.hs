{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE OverloadedLabels           #-}
{-# LANGUAGE OverloadedLists            #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE FlexibleInstances          #-}

{-# OPTIONS_GHC -Wno-ambiguous-fields #-}
module IOHK.Certification.Persistence.Structure.Invoicing where
import Database.Selda
import Data.Int
import IOHK.Certification.Persistence.Pattern
import IOHK.Certification.Persistence.Structure.Subscription
import IOHK.Certification.Persistence.Structure.Internal
import IOHK.Certification.Persistence.Structure.Profile
import Data.Aeson
import Data.Swagger
import Data.Proxy
import Control.Lens hiding ((.=),index)
import Data.HashMap.Strict.InsOrd as HM
import qualified Data.Aeson.KeyMap as KM

--data Invoice = Invoice
  --{
  --, invDate           :: UTCTime
  --, invAdaUsdPrice    :: Double
  --} deriving (Generic, Show, Eq)
data Invoice = Invoice
  { invId             :: ID Invoice
  , invDate           :: UTCTime
  , invProfileId      :: ID Profile
  , invOwnerAddress   :: ProfileWalletAddress
  , invWebsite        :: Maybe Website
  , invEmail          :: Maybe Email
  , invCompanyName    :: Text
  , invFullName       :: Text
  , invAdaUsdPrice    :: Double
  , invCancelledInvId :: Maybe (ID Invoice)
  , invPrinted        :: Int64
  } deriving (Generic, Show, Eq)

instance SqlRow Invoice

type InvoiceId = ID Invoice

instance FromJSON  (ID Invoice) where
  parseJSON = withScientific "InvoiceId" $
    pure . toId . scientificToInt64

instance ToJSON (ID Invoice) where
  toJSON = toJSON . fromId

instance ToSchema (ID Invoice) where
  declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy Int64)

instance ToParamSchema (ID Invoice) where
  toParamSchema _ = mempty
    & type_ ?~ SwaggerInteger

instance FromJSON Invoice where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = dropAndLowerFirst 3 }

instance ToJSON Invoice where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = dropAndLowerFirst 3 }

instance ToSchema Invoice where
  declareNamedSchema = genericDeclareNamedSchema defaultSchemaOptions { fieldLabelModifier = dropAndLowerFirst 3 }

data InvoiceItem = InvoiceItem
  { invItemInvId         :: ID Invoice
  , invItemIx            :: Int
  , invItemName          :: Text
  , invItemCount         :: Int
  , invItemUnitPrice     :: Int64
  , invItemVatPercentage :: Int64
  } deriving (Generic, Show, Eq)

instance SqlRow InvoiceItem

instance FromJSON InvoiceItem where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = dropAndLowerFirst 7}

instance ToJSON InvoiceItem where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = dropAndLowerFirst 7}

instance ToSchema InvoiceItem where
  declareNamedSchema = genericDeclareNamedSchema defaultSchemaOptions { fieldLabelModifier = dropAndLowerFirst 7 }

data SubscriptionInvoice = SubscriptionInvoice
  { subInvId    :: ID SubscriptionInvoice
  , subInvInvId :: ID Invoice
  , subInvSubId :: ID Subscription
  } deriving (Generic, Show, Eq)

instance SqlRow SubscriptionInvoice

invoices :: Table Invoice
invoices = tableFieldMod "invoice"
  [ #invId :- autoPrimary
  , #invDate :- index
  , #invProfileId :- foreignKey profiles #profileId
  , #invCancelledInvId :- foreignKey invoices #invId
  ] (dropPrefix "inv")

invoiceItems :: Table InvoiceItem
invoiceItems = tableFieldMod "invoice_item"
  [ #invItemInvId :- foreignKey invoices #invId
  ] (dropPrefix "invItem")

subscriptionInvoices :: Table SubscriptionInvoice
subscriptionInvoices = tableFieldMod "subscription_invoice"
  [ #subInvId :- autoPrimary
  , #subInvInvId :- foreignKey invoices #invId
  , #subInvSubId :- foreignKey subscriptions #subscriptionId
  ] (dropPrefix "subInv")

data InvoiceDTO = InvoiceDTO
  { invDtoParent :: Invoice
  , invDtoItems :: [InvoiceItemDTO]
  } deriving (Eq, Show, Generic)

newtype InvoiceItemDTO = InvoiceItemDTO
  { invItemDto :: InvoiceItem
  } deriving (Eq, Show, Generic)

instance FromJSON InvoiceDTO where
  parseJSON = withObject "InvoiceDTO" $ \v ->do
    invoice <- parseJSON (Object v)
    rawItems <- v .:? "items" .!= []
    -- replace the invId field with the one from the parent
    let items' = Prelude.map (\(InvoiceItemDTO item) ->
          InvoiceItemDTO item { invItemInvId = invId invoice }) rawItems
    pure $ InvoiceDTO invoice items'

instance ToJSON InvoiceDTO where
  toJSON (InvoiceDTO invoice items') = Object (x <> y)
    where
    x = KM.fromList [ "items" .= items' ]
    -- remove the id field
    y = case toJSON invoice of
      Object obj -> obj
      _          -> KM.empty

instance ToSchema InvoiceDTO where
  declareNamedSchema _ = do
    invoiceSchema <- declareSchema (Proxy :: Proxy Invoice)
    invoiceItemsItemSchema <- declareSchemaRef (Proxy :: Proxy [InvoiceItemDTO])
    pure $ NamedSchema (Just "InvoiceDTO") $ invoiceSchema
        -- remove invId from properties
      & properties %~ (`mappend`
          [ ("items", invoiceItemsItemSchema) ])

instance FromJSON InvoiceItemDTO where
  parseJSON = withObject "InvoiceItemDTO" $ \v -> do
    let v' = KM.insert "invId" (toJSON (-1 :: Int64)) v
    InvoiceItemDTO <$> parseJSON (Object v')

instance ToJSON InvoiceItemDTO where
  toJSON (InvoiceItemDTO item) = Object y
    where
    -- remove the invId field
    y = case toJSON item of
      Object obj -> KM.delete "invId" obj
      _          -> KM.empty

instance ToSchema InvoiceItemDTO where
  declareNamedSchema _ = do
    invoiceItemSchema <- declareSchema (Proxy :: Proxy InvoiceItem)
    pure $ NamedSchema (Just "InvoiceItemDTO") $ invoiceItemSchema
        -- remove invId from properties
        & properties %~ HM.delete "invId"
        -- remove invId from required
        & required %~ Prelude.filter (/= "invId")

data InvoiceBody = InvoiceBody
  { invBodyWebsite        :: Maybe Website
  , invBodyEmail          :: Maybe Email
  , invBodyCompanyName    :: Maybe Text
  , invBodyFullName       :: Maybe Text
  , invBodyItems          :: [InvoiceItemBody]
  } deriving (Eq, Show, Generic)

newtype InvoiceItemBody = InvoiceItemBody
  { invItemDto :: InvoiceItem
  } deriving (Eq, Show, Generic)

instance FromJSON InvoiceBody where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = dropAndLowerFirst 7 }

instance ToJSON InvoiceBody where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = dropAndLowerFirst 7 }

instance ToSchema InvoiceBody where
  declareNamedSchema = genericDeclareNamedSchema defaultSchemaOptions { fieldLabelModifier = dropAndLowerFirst 7 }

instance FromJSON InvoiceItemBody where
  parseJSON = withObject "InvoiceItemBody" $ \v -> do
    let v' = KM.insert "invId" (toJSON (-1 :: Int64)) v
    InvoiceItemBody <$> parseJSON (Object v')

instance ToJSON InvoiceItemBody where
  toJSON (InvoiceItemBody item) = Object y
    where
    -- remove the invId field
    y = case toJSON item of
      Object obj -> KM.delete "invId" obj
      _          -> KM.empty

instance ToSchema InvoiceItemBody where
  declareNamedSchema _ = do
    invoiceItemSchema <- declareSchema (Proxy :: Proxy InvoiceItem)
    pure $ NamedSchema (Just "InvoiceItemBody") $ invoiceItemSchema
        -- remove invId from properties
        & properties %~ HM.delete "invId"
        -- remove invId from required
        & required %~ Prelude.filter (/= "invId")

