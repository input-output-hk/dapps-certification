import Test.Hspec
import qualified ProfileWalletSpec as ProfileWallet
import qualified SignatureSpec as Signature

main :: IO ()
main = hspec $ do
  ProfileWallet.spec
  Signature.spec
