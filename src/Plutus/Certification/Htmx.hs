{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE QuasiQuotes          #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE NamedFieldPuns   #-}

module Plutus.Certification.Htmx where

import Plutus.Certification.Htmx.Internal
import NeatInterpolation (text)
import Data.Text as Text (take, drop, length,Text,pack)
-- import Persistence
import qualified IOHK.Certification.Persistence as DB

import Prelude hiding (length,drop)
import Data.Maybe (fromMaybe)
import Data.Time
import Control.Arrow

toText :: Show a => a -> Text
toText = pack . show

truncateText :: Int -> Text -> Text
truncateText maxLength text'
  | length text' <= maxLength = text'
  | otherwise = let
      half = maxLength `div` 2
      left' = Text.take half text'
      right' = Text.take half (drop (length text' - half) text')
    in left' <> "..." <> right'

renderSummaryTrDetailed :: DB.ProfileSummaryDTO -> Text
renderSummaryTrDetailed summary@DB.ProfileSummaryDTO{..}  =

  renderSummaryTr summary <> renderDetailed
  where
  renderDetailed =
    let profileId'  = toText $ DB.fromId $ summaryProfile.profileId
    in [text|
    <tr id="pidId_${profileId'}_detailed"
      class="border-b transition-colors hover:bg-muted/50 data-[state=selected]:bg-muted"
    >
    CEVA
    </tr>
  |]

renderSummaryTr :: DB.ProfileSummaryDTO -> Text
renderSummaryTr DB.ProfileSummaryDTO{..} =
  [textF|src/Plutus/Certification/Htmx/Template/profile-summary-tr.html|]
  where
    DB.Profile{..} = summaryProfile
    profileId'  = toText $ DB.fromId $ summaryProfile.profileId
    address' = truncateText 30 $ DB.getPatternedText ownerAddress
    (dapp',dappRepo') = case summaryDapp of
      Nothing -> ("-","-")
      Just (DB.DAppDTO DB.DApp{..}) ->
        let name = truncateText 30 dappName
            repo = "https://github.com/" <> dappOwner <> "/" <> dappRepo
        in (name,repo)
    totalRuns' = case summaryRunStats.runsTotal of
          0 -> "-"
          n -> toText n
    roleLabel = case summaryMaxRole of
          DB.NoRole -> Nothing
          DB.Support -> Just $ Label Green "Support"
          DB.Admin -> Just $ Label Orange "Admin"
    subscriptionComponent = maybe "" label $ case summarySubscription of
          Nothing -> Nothing
          Just (DB.SubscriptionLite{..}) -> case subTierType of
            DB.Developer -> Just $ Label Gray "Developer"
            DB.Auditor -> Just $ Label Blue "Auditor"
    roleComponent = maybe "" label roleLabel
    labels' = roleComponent <> "\n " <> subscriptionComponent
    (failedRuns,abortedRuns,succeededRuns) = case summaryRunStats of
      _ | summaryRunStats.runsTotal == 0 -> ( "-","-","-")
      (DB.RunStats{..}) -> (toText runsFailed, toText runsAborted, toText runsSuccessful)
    (subscriptionStatus,subscribedOn,expiresOn) = case summarySubscription of
      Nothing -> ("-","-","-")
      Just (DB.SubscriptionLite{..}) ->
        (subStatusT subStatus,time' subStartDate,time' subEndDate)
    companyName'= fromMaybe "-" companyName

subStatusT :: DB.SubscriptionStatus -> Text
subStatusT DB.ActiveSubscription = "Active"
subStatusT DB.InactiveSubscription = "Inactive"
subStatusT DB.PendingSubscription = "Pending"

time' :: UTCTime -> Text
time' = pack . formatTime defaultTimeLocale "%m/%d/%Y"

data LabelColor = Red | Green | Blue | Orange | Gray

data Label= Label
  { lblColor :: LabelColor
  , lblText :: Text
  }

label :: Label -> Text
label Label{..} =
  let color = case lblColor of
        Blue -> "blue"
        Red -> "red"
        Green -> "green"
        Orange -> "orange"
        Gray -> "gray"
      (bgColor,fgColor) = (color <> "-200", color <> "-800")
  in [textF|src/Plutus/Certification/Htmx/Template/label.html|]

type BtnLabel = Text
data AsideIcon = Accounts | Billing | Home | Transactions
               deriving (Eq,Show)
type Selected = Bool

asideButton :: AsideIcon -> Selected -> Text
asideButton icon selected =
  [textF|src/Plutus/Certification/Htmx/Template/aside-button.html|]
  where
  icon' = case icon of
    Accounts -> [textF|src/Plutus/Certification/Htmx/Template/Icons/accounts.html|]
    Billing -> [textF|src/Plutus/Certification/Htmx/Template/Icons/billing.html|]
    Home -> [textF|src/Plutus/Certification/Htmx/Template/Icons/home.html|]
    Transactions -> [textF|src/Plutus/Certification/Htmx/Template/Icons/transactions.html|]
  (label',path) = case icon of
    Accounts -> ("Accounts","accounts")
    Billing -> ("Billing","billing")
    Home -> ("Home","")
    Transactions -> ("Transactions","transactions")
  extraStyle = if selected then "bg-gray-200 text-gray-800"
                           else "hover:bg-gray-200 text-gray-500"

accountsPage :: [DB.ProfileSummaryDTO] -> Text
accountsPage allSummaryRows =
  withMasterPage Accounts [textF|src/Plutus/Certification/Htmx/Template/accounts.html|]
  where
   th str = [text|
     <th
        class="h-12 px-4 text-left align-middle font-medium text-muted-foreground [&amp;:has([role=checkbox])]:pr-0"
      >
        $str
      </th> |]
   tableHeader = mconcat $ map th ["Address", "DApp", "Category", "Total Runs",""]
   allSummaryTr = mconcat $ map renderSummaryTr allSummaryRows

transactionsPage :: Text
transactionsPage = withMasterPage Transactions [textF|src/Plutus/Certification/Htmx/Template/transactions.html|]

billingPage :: Text
billingPage = withMasterPage Billing [textF|src/Plutus/Certification/Htmx/Template/billing.html|]

homePage :: Text
homePage = withMasterPage Home [textF|src/Plutus/Certification/Htmx/Template/home.html|]

type SubPage = Text

withMasterPage :: AsideIcon -> SubPage -> Text
withMasterPage selectedPage subPage =
  [textF|src/Plutus/Certification/Htmx/Template/master.html|]
  where
  asideButtons = mconcat $ map
    (uncurry asideButton . (id &&& (== selectedPage)))
    [ Home
    , Accounts
    , Transactions
    , Billing
    ]
  pageName = case selectedPage of
    Home -> "Home"
    Accounts -> "Accounts"
    Transactions -> "Transactions"
    Billing -> "Billing"
