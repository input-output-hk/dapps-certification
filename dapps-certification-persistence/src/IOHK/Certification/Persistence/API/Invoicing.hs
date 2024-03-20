{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE NamedFieldPuns        #-}

module IOHK.Certification.Persistence.API.Invoicing where

import Data.Int
import Data.Maybe
import Database.Selda hiding (Set)
import IOHK.Certification.Persistence.Structure.Profile
import IOHK.Certification.Persistence.Structure.Invoicing
import IOHK.Certification.Persistence.Structure.Subscription
import IOHK.Certification.Persistence.Structure.Internal
import IOHK.Certification.Persistence.API.Profile
import Control.Monad (forM)
import Control.Applicative

import qualified Data.Text as Text

-- | get all invoices in a period
--
-- @param start start date
--
-- @param end end date
--
-- @return all invoices in the period
getAllInvoices :: MonadSelda m
               => Maybe UTCTime
               -> Maybe UTCTime
               -> m [InvoiceDTO]
getAllInvoices = getInvoices' Nothing

getAllInvoiceIds :: MonadSelda m => m [InvoiceId]
getAllInvoiceIds = query $ do
    inv <- select invoices
    pure (inv ! #invId)

-- | get all unprinted invoices
--
-- @return all unprinted invoices
getUnprintedInvoices :: MonadSelda m => m [InvoiceId]
getUnprintedInvoices = query $ do
    inv <- select invoices
    restrict (inv ! #invPrinted .== literal (0 :: Int64))
    pure (inv ! #invId)

-- | get all invoices for a given profile in a period
--
-- @param profileId profile id
--
-- @param start start date
--
-- @param end end date
--
-- @return all invoices for the profile in the period
getProfileInvoices :: MonadSelda m
                      => ProfileId
                      -> Maybe UTCTime
                      -> Maybe UTCTime
                      -> m [InvoiceDTO]
getProfileInvoices profileId = getInvoices' (Just profileId)

getInvoice :: MonadSelda m
           => InvoiceId
           -> m (Maybe InvoiceDTO)
getInvoice invId' = do
  invM <- listToMaybe <$> query  (do
    inv <- select invoices
    restrict (inv ! #invId .== literal invId')
    pure inv)
  itemsM <- forM invM $ \Invoice{invId} -> query (getInvoiceItems invId)
  case (invM,itemsM) of
    (Just inv, Just items) -> pure $ Just $ toDTO inv items
    _ -> pure Nothing

getInvoices' :: MonadSelda m
             => Maybe ProfileId
             -> Maybe UTCTime
             -> Maybe UTCTime
             -> m [InvoiceDTO]
getInvoices' profileIdM start end = do
  -- get all invoices for the profile
  invoices' <- query $ do
        inv <- select invoices
        case profileIdM of
          Nothing -> pure ()
          Just profileId -> restrict (inv ! #invProfileId .== literal profileId)
        mapM_ (\s -> restrict (inv ! #invDate .>= literal s)) start
        mapM_ (\e -> restrict (inv ! #invDate .< literal e)) end
        pure inv
  -- get all invoice items for the invoices
  items <- forM invoices' $ \Invoice{invId} -> query (getInvoiceItems invId)
  pure $ zipWith toDTO invoices' items

toDTO :: Invoice -> [InvoiceItem] -> InvoiceDTO
toDTO inv items = InvoiceDTO inv (Prelude.map InvoiceItemDTO items)

data CreateInvoiceResult
  = CreateInvoiceResultProfileNotFound
  | CreateInvoiceResultRequiredFieldMissing Text.Text
  | CreateInvoiceResultInvoice InvoiceDTO

getInvoiceItems :: ID Invoice -> Query t (Row t InvoiceItem)
getInvoiceItems invId' = do
    items <- select invoiceItems
    restrict (items ! #invItemInvId .== literal invId')
    pure items
-- | create an invoice for a profile
--
-- @param inv invoice to create
--
-- @param items items to create
createInvoice :: MonadSelda m
              => ProfileId
              -> UTCTime
              -> AdaUsdPrice
              -> InvoiceBody
              -> m CreateInvoiceResult
createInvoice profileId now adaUsdPrice InvoiceBody{..} = do
  -- fetch the profile
  profileM <- getProfile' profileId
  case (profileM,invBodyCompanyName,invBodyFullName) of
    -- if the profile is not found return an error
    (Nothing,_,_) -> pure CreateInvoiceResultProfileNotFound

    -- if the profile is found but company name or full name is missing both
    -- from the body and the profile return an error
    (Just Profile{companyName},Nothing,_) | isNothing companyName ->
      pure $ CreateInvoiceResultRequiredFieldMissing "companyName"
    (Just Profile{fullName},_,Nothing) | isNothing fullName ->
      pure $ CreateInvoiceResultRequiredFieldMissing "fullName"

    -- otherwise start creating the invoice
    (Just Profile{profileId=_,..},_,_) -> do
      -- fill in all the fields based on the body and the profile
      let inv = Invoice
            { invId = def
            , invDate = now
            , invProfileId = profileId
            , invOwnerAddress = ownerAddress
            , invAdaUsdPrice = realToFrac adaUsdPrice
            , invCancelledInvId = Nothing
            , invWebsite = invBodyWebsite <|> website
            , invEmail = invBodyEmail <|> email <|> contactEmail
            , invFullName = fromMaybe (fromJust fullName) invBodyFullName
            , invCompanyName = fromMaybe (fromJust companyName) invBodyCompanyName
            , invPrinted = 0
            }
      -- insert the invoice and get the id
      invId <- insertWithPK invoices [inv]
      -- attach the invoice id to the items
      let invItems = Prelude.map (fromBodyItemsToItems invId) invBodyItems
      -- insert the items
      _ <- insert_ invoiceItems invItems

      -- return the invoiceDto
      pure $ CreateInvoiceResultInvoice
        (InvoiceDTO inv (Prelude.map InvoiceItemDTO invItems))
  where
  fromBodyItemsToItems :: InvoiceId -> InvoiceItemBody -> InvoiceItem
  fromBodyItemsToItems invId (InvoiceItemBody item) = item { invItemInvId = invId}

data CancelInvoiceResult
  = InvoiceNotFound
  | InvoiceAlreadyCanceled
  | InvoiceCanceled InvoiceDTO
  deriving (Show, Eq)

-- | Cancel an invoice for a profile by creating a new one with negative values
-- it returns the new cancellation invoice
--
-- @param invId' invoice id to cancel
--
-- @return the new cancellation invoice or an error
cancelInvoice :: (MonadSelda m,MonadMask m)
              => InvoiceId
              -> m CancelInvoiceResult
cancelInvoice invId' = transaction $ do
  -- first search if the invoice is already canceled
  notCanceled <-  null <$> query ( do
    inv <- select invoices
    restrict (inv ! #invCancelledInvId .== literal (Just invId'))
    return inv)

  -- find the invoice to cancel
  invM <- listToMaybe <$> query (do
    inv <- select invoices
    restrict (inv ! #invId .== literal invId')
    pure inv)

  case (notCanceled,invM) of
    (False,_) -> pure InvoiceAlreadyCanceled
    (_,Nothing) -> pure InvoiceNotFound
    (_,Just inv) -> InvoiceCanceled <$> cancelInvoice' inv
  where

  -- create a new invoice with negative values
  cancelInvoice' :: MonadSelda m
                 => Invoice
                 -> m InvoiceDTO
  cancelInvoice' inv = do
    -- get all items for the invoice
    items <- query $ do
      item <- select invoiceItems
      restrict (item ! #invItemInvId .== literal invId')
      pure item

    -- prepare items for the new invoice
    let newInv = inv { invId = def, invCancelledInvId = Just invId' }

    -- insert the new invoice and items
    cancellationInvId <- insertWithPK invoices [newInv]

    -- insert the new items
    let items' = map (canceledItem cancellationInvId) items
    insert_ invoiceItems items'

    -- return the new invoice and items
    pure $ InvoiceDTO
      newInv { invId = cancellationInvId }
      (Prelude.map InvoiceItemDTO items')

  -- negate the unit price of the item and set the cancellation invoice id
  canceledItem :: InvoiceId -> InvoiceItem -> InvoiceItem
  canceledItem cancellationInvId item  = item
    { invItemInvId = cancellationInvId
    , invItemUnitPrice = negate (invItemUnitPrice item)
    }

data CreateSubscriptionInvoiceResult
  = SubscriptionNotFound
  | SubscriptionStatusNotCompatible
  | SubscriptionAlreadyInvoiced
  | SubscriptionMissingProfileData String
  | SubscriptionInvoiced InvoiceDTO

type VatPercentage = Int64

isProfileReadyForInvoicing :: MonadSelda m => ProfileId -> m Bool
isProfileReadyForInvoicing pid = do
  profileM <- getProfile' pid
  case profileM of
    Nothing -> pure False
    Just Profile{..} -> do
      pure $ isJust companyName && isJust fullName

-- | create an invoice for a subscription
--
-- @param subId subscription id
-- @param vat vat percentage
-- @return the new invoice or an error
createSubscriptionInvoice :: MonadSelda m
                          => SubscriptionId
                          -> Int64
                          -> m CreateSubscriptionInvoiceResult
createSubscriptionInvoice subId vat = do

  -- get the subscription
  subscriptionM <- listToMaybe <$> query (do
    sub <- select subscriptions
    restrict (sub ! #subscriptionId .== literal subId)
    pure sub)

  -- check if the subscription has been already been invoiced
  alreadyInvoiced' <- alreadyInvoiced

  case (subscriptionM,alreadyInvoiced') of
    (_,True) -> pure SubscriptionAlreadyInvoiced

    (Nothing,_) -> pure SubscriptionNotFound

    -- check if the subscription has been paid. That means if it's active or inactive
    -- The Inactive one means that the subscription has been active at some point,
    -- so it can and it should be invoiced
    (Just (Subscription{..}),_)
      | subscriptionStatus /= ActiveSubscription
        && subscriptionStatus /= InactiveSubscription
        -> pure SubscriptionStatusNotCompatible

      | otherwise -> do
        -- get the profile
        Profile{..} <- fromJust <$> getProfile' subscriptionProfileId
        case (companyName,fullName) of
          (Nothing,_) -> pure $ SubscriptionMissingProfileData "companyName"
          (_,Nothing) -> pure $ SubscriptionMissingProfileData "fullName"
          (Just companyName',Just fullName') -> do
            -- create the invoice based on the subscription
            let inv = Invoice
                  { invId = def
                  , invProfileId = subscriptionProfileId
                  , invDate = subscriptionStartDate
                  , invCancelledInvId = Nothing
                  , invOwnerAddress = ownerAddress
                  , invWebsite = website
                  , invEmail = email <|> contactEmail
                  , invCompanyName = companyName'
                  , invFullName = fullName'
                  , invAdaUsdPrice = subscriptionAdaUsdPrice
                  , invPrinted = 0
                  }
            invId <- insertWithPK invoices [inv]
            let invItem = InvoiceItem
                  { invItemInvId = invId
                  , invItemIx = 0
                  , invItemName = "Subscription - " <> Text.pack (show subscriptionType)
                  , invItemUnitPrice = subscriptionPrice
                  , invItemCount = 1
                  , invItemVatPercentage = vat
                  }
            insert_ invoiceItems [invItem]
            pure $ SubscriptionInvoiced (InvoiceDTO inv { invId } [InvoiceItemDTO invItem])

  where
  alreadyInvoiced :: MonadSelda m => m Bool
  alreadyInvoiced = do
    -- get all invoices for this subscription and their cancellations
    allInvoices <- query subInvoicesAndCancellations

    -- filter out those not canceled
    pure $ any (\(_ :*: cancellation) -> isNothing cancellation) allInvoices

  subInvoicesAndCancellations :: Query t (Row t SubscriptionInvoice :*: Row t (Maybe Invoice))
  subInvoicesAndCancellations  = do
    subInv <- select subscriptionInvoices
    restrict (subInv ! #subInvSubId .== literal subId)

    -- now for the pointed invoice see if it's canceled
    cancellation <- leftJoin
      (\canceled -> canceled ! #invCancelledInvId .== just (subInv ! #subInvInvId))
      (select invoices)
    pure (subInv :*: cancellation)

-- increase subscription counter for the invoice
increaseInvoicePrintCounter :: MonadSelda m => InvoiceId -> m ()
increaseInvoicePrintCounter  invId' = do
  update_ invoices
    (\inv -> inv ! #invId .== literal invId')
    (\inv -> inv `with` [ #invPrinted := inv ! #invPrinted + 1 ])

