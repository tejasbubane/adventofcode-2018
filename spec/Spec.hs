import Test.Hspec

import qualified Day1Spec
import qualified Day2Spec

main :: IO ()
main = hspec $ do
  Day1Spec.specs
  Day2Spec.specs
