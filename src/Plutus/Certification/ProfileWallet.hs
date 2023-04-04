module Plutus.Certification.ProfileWallet where

import Data.Text as T
import Data.Map
asda = "asd"



-- TODO: find a better place to put this
-- there is already one defined in other places
newtype PublicAddress = PublicAddress { unPublicAddress :: Text }
                      deriving (Eq,Show)

data ProfileWallet = ProfileWallet
    { pwAddress :: PublicAddress
    } deriving (Eq, Show)


--ty ProfileMap = 
