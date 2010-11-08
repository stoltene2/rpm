
import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck (testProperty)

import CombinatorsTest
import InfoTest



-- hpc markup --exclude=Main --exclude=CombinatorsTest --destdir=doc/hpc test-rpm

main :: IO ()
main = defaultMain tests

tests = [
        testGroup "Combinator Properties" 
            [ 
              testProperty "Multiplicative Identity"  prop_MultiplicativeIdentity
            , testProperty "Additive Identity"        prop_AdditiveIdentity
            , testProperty "Reflexive And with zeros" prop_ReflexiveAndZeros
            , testProperty "Reflexive And with ones"  prop_ReflexiveAndOnes
            , testProperty "Commutativity of And"     prop_CommuativeAnd
            , testProperty "Commutativity of Or"      prop_CommuativeOr
            , testProperty "Associativity of Or"      prop_AssociativeOr
            , testProperty "Associativity of And"     prop_AssociativeAnd
            , testProperty "Distributivity of Or"     prop_DistributiveOr
            , testProperty "Distributivity of And"    prop_DistributiveAnd
            , testProperty ".>." prop_GreaterThan 
            , testProperty ".>=." prop_GreaterThanEq 
            , testProperty ".<." prop_LessThan 
            , testProperty ".<=." prop_LessThanEq 
            , testProperty ".<=. && .>." prop_LessThanEqAndGreaterThan 
            , testProperty "./=." prop_NotEqual
            , testProperty "notP" prop_Not
            , testProperty "notP again" prop_Not2
            , testProperty "notP again, again" prop_Not3
            , testProperty "notP notP" prop_Not4
            , testCase "Not all fields present." test_notAllFieldsPresent
            , testCase "Some empty tags" test_containsEmptyTags
            ]
        ]



