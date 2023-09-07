import Test.Hspec
import qualified ProfileWalletSpec as ProfileWallet
import qualified SignatureSpec as Signature
import qualified PatternSpec as Pattern
import qualified JSONSpec as JSON

main :: IO ()
main = hspec $ do
  ProfileWallet.spec
  Signature.spec
  Pattern.spec
  JSON.spec
