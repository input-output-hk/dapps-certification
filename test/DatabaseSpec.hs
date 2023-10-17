module DatabaseSpec (spec) where

import Test.Hspec
import qualified Database.Migration as Migration

spec :: SpecWith ()
spec = describe "Database" $ do
  Migration.spec

