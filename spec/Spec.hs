import Test.Hspec

import qualified Day1Spec
import qualified Day2Spec
import qualified Day3Spec
import qualified Day4Spec
import qualified Day5Spec
import qualified Day6Spec
import qualified Day14Spec
import qualified Day16Spec

main :: IO ()
main = hspec $ do
  Day1Spec.specs
  Day2Spec.specs
  Day3Spec.specs
  Day4Spec.specs
  Day5Spec.specs
  Day6Spec.specs
  Day14Spec.specs
  Day16Spec.specs
