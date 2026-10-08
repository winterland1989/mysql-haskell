-- | 'readDouble' and 'readFloat' must give the nearest value, as 'read' does,
-- for every way MySQL writes a FLOAT or DOUBLE: plain decimals, and exponent
-- form for large values, whether the fast path or the exact one reads them.
module FloatingPointText (tests) where

import qualified Data.ByteString.Char8                 as BC
import           Database.MySQL.Protocol.FloatingPoint (readDouble, readFloat)
import           Numeric                               (showFFloat)
import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.HUnit                      (testCase, (@?=))
import           Test.Tasty.QuickCheck                 (testProperty)

tests :: TestTree
tests = testGroup "FLOAT and DOUBLE text"
    [ testProperty "a shown Double reads back as itself" $
        forAll finiteDouble $ \value -> readDouble (BC.pack (show value)) === Just value
    , testProperty "a Double in plain decimals reads back as itself" $
        forAll finiteDouble $ \value -> readDouble (BC.pack (showFFloat Nothing value "")) === Just value
    , testProperty "a shown Float reads back as itself" $
        forAll finiteFloat $ \value -> readFloat (BC.pack (show value)) === Just value
    , testProperty "any decimal text gives the Double read gives" $
        forAll genDecimalText $ \text -> readDouble (BC.pack text) === Just (read text)
    , testProperty "any decimal text gives the Float read gives" $
        forAll genDecimalText $ \text -> readFloat (BC.pack text) === Just (read text)
    , testCase "exponent form, as MySQL writes large values" $
        map (readDouble . BC.pack) ["1e20", "-1e20", "1.5e300", "2.5E-7", "1e+5"]
            @?= map Just [1e20, -1e20, 1.5e300, 2.5e-7, 1e5]
    , testCase "seventeen significant digits" $
        readDouble (BC.pack "0.30000000000000004") @?= Just (0.1 + 0.2)
    , testCase "out of range" $
        map (readDouble . BC.pack) ["1e400", "1e-400"] @?= map Just [1 / 0, 0]
    , testCase "bytes after the number are ignored, as readDecimal ignored them" $
        map (readDouble . BC.pack) ["12abc", "3.5e", "7.", "1e5x"] @?= map Just [12, 3.5, 7, 1e5]
    , testCase "text that does not start with a number" $
        map (readDouble . BC.pack) ["", "abc", ".5", "-", "e5"] @?= replicate 5 Nothing
    ]

finiteDouble :: Gen Double
finiteDouble = arbitrary `suchThat` (\value -> not (isNaN value || isInfinite value))

finiteFloat :: Gen Float
finiteFloat = arbitrary `suchThat` (\value -> not (isNaN value || isInfinite value))

-- | A sign, 1 to 25 digits, an optional fraction and an optional exponent from
-- -330 to 330: short numbers take the fast path, long ones and large
-- exponents the exact one.
genDecimalText :: Gen String
genDecimalText = do
    sign <- elements ["", "-"]
    integerDigits <- choose (1, 25) >>= flip vectorOf (elements ['0' .. '9'])
    fraction <- oneof [pure "", ('.' :) <$> (choose (1, 20) >>= flip vectorOf (elements ['0' .. '9']))]
    exponentPart <- oneof [pure "", ('e' :) . show <$> choose (-330, 330 :: Int)]
    pure (sign ++ integerDigits ++ fraction ++ exponentPart)
