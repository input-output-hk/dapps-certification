import Test.Hspec
import Test.Hspec.QuickCheck (modifyMaxSuccess)

import qualified ProfileWalletSpec as ProfileWallet
import qualified SignatureSpec as Signature
import qualified PatternSpec as Pattern
import qualified JSONSpec as JSON
import qualified DatabaseSpec as Database

main :: IO ()
main = hspec $ modifyMaxSuccess (const 1000) $ do
  Signature.spec
  Pattern.spec
  JSON.spec
  Database.spec
  ProfileWallet.spec
