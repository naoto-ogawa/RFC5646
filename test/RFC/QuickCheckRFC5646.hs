module RFC.QuickCheckRFC5646 where

import Test.Tasty
import Test.Tasty.QuickCheck as QC
--
import RFC.Internal.RFC5646

-- for auto-discovery
test_qc = tests

-- for ghci test
main = defaultMain tests

tests :: TestTree
tests = testGroup "QC test" [properties]

properties :: TestTree
properties = testGroup "Properties" [qcProps]

qcProps = testGroup "formating after reading of a input is the same as the input "
  [ QC.testProperty "two letters" $
     \(Lang1 x) -> check x 
  , QC.testProperty "three letters" $
     \(Lang2 x) -> check x 
  , QC.testProperty "two-three letters" $
     \(Lang3 x) -> check x 
  ]

check x = format (read x :: LanguageTag ) == x

data Lang1 = Lang1 String deriving (Show, Eq, Ord)
data Lang2 = Lang2 String deriving (Show, Eq, Ord)
data Lang3 = Lang3 String deriving (Show, Eq, Ord)

instance Arbitrary Lang1 where
  arbitrary = do
    x <- vectorOf 2 $ choose ('a','z')
    -- x <- vectorOf 2 $ choose ('A','Z')
    return $ Lang1 x

instance Arbitrary Lang2 where
  arbitrary = do
    x <- vectorOf 3 $ choose ('a','z')
    return $ Lang2 x

instance Arbitrary Lang3 where
  arbitrary = do
    Lang1 x <- arbitrary
    Lang2 y <- arbitrary
    return $ Lang3 $ x ++ "-" ++ y
