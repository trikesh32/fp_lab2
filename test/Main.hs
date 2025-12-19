module Main (main) where

import Control.Monad (unless)
import Data.Bifunctor (second)
import Data.Maybe (isNothing)
import Structure
import System.Exit (exitFailure)
import Test.HUnit hiding (Testable)
import Test.QuickCheck

newtype StringDict = StringDict {getDict :: PrefixTreeDict Int}
  deriving (Show)

instance Arbitrary StringDict where
  arbitrary = StringDict . fromListDict <$> arbitrary
  shrink (StringDict dict) = StringDict . fromListDict <$> shrink (toListDict dict)

fromListDict :: [(String, Int)] -> PrefixTreeDict Int
fromListDict = foldr (\(k, v) acc -> insertDict k v acc) mempty

toListDict :: PrefixTreeDict Int -> [(String, Int)]
toListDict = foldrWithKey (\k v acc -> (k, v) : acc) []

testInsertAndLookup :: Test
testInsertAndLookup = TestCase $ do
  let dict = insertDict "hello" 42 mempty :: PrefixTreeDict Int
  assertEqual "Inserted value should be retrievable" (Just 42) (lookupDict "hello" dict)
  assertEqual "Non-existent key should return Nothing" Nothing (lookupDict "world" dict)

testDeleteRemovesKey :: Test
testDeleteRemovesKey = TestCase $ do
  let dict :: PrefixTreeDict Int
      dict = deleteDict "apple" (fromListDict [("apple", 1), ("app", 2), ("application", 3)])
  assertBool "Deleted key should not be present" (not (memberDict "apple" dict))
  assertBool "Prefix should still exist" (memberDict "app" dict)
  assertBool "Longer key should still exist" (memberDict "application" dict)

testFilterKeepsMatching :: Test
testFilterKeepsMatching = TestCase $ do
  let dict :: PrefixTreeDict Int
      dict = fromListDict [("a", 1), ("ab", 2), ("abc", 3), ("b", 4)]
      filtered = filterDict (\k _ -> case k of (c:_) -> c == 'a'; [] -> False) dict
  assertBool "Keys starting with 'a' should be present" (memberDict "a" filtered)
  assertBool "Keys starting with 'a' should be present" (memberDict "ab" filtered)
  assertBool "Keys starting with 'a' should be present" (memberDict "abc" filtered)
  assertBool "Keys not starting with 'a' should be absent" (not (memberDict "b" filtered))

testFoldAggregatesValues :: Test
testFoldAggregatesValues = TestCase $ do
  let dict :: PrefixTreeDict Int
      dict = fromListDict [("one", 1), ("two", 2), ("three", 3)]
      sumValues = foldlWithKey (\acc _ v -> acc + v) 0 dict
  assertEqual "Fold should sum all values" 6 sumValues

testMapTransformsValues :: Test
testMapTransformsValues = TestCase $ do
  let dict :: PrefixTreeDict Int
      dict = fromListDict [("a", 1), ("b", 2), ("c", 3)]
      mapped = fmap (* 10) dict
  assertEqual "Mapped value should be transformed" (Just 10) (lookupDict "a" mapped)
  assertEqual "Mapped value should be transformed" (Just 20) (lookupDict "b" mapped)
  assertEqual "Mapped value should be transformed" (Just 30) (lookupDict "c" mapped)

testPrefixBehavior :: Test
testPrefixBehavior = TestCase $ do
  let dict :: PrefixTreeDict Int
      dict = fromListDict [("test", 1), ("testing", 2), ("tester", 3)]
  assertBool "Prefix 'test' should exist" (memberDict "test" dict)
  assertBool "Key 'testing' should exist" (memberDict "testing" dict)
  assertBool "Key 'tester' should exist" (memberDict "tester" dict)
  assertBool "Non-existent prefix should not exist" (not (memberDict "tes" dict))

unitTests :: Test
unitTests =
  TestList
    [ TestLabel "insertAndLookup" testInsertAndLookup,
      TestLabel "delete" testDeleteRemovesKey,
      TestLabel "filter" testFilterKeepsMatching,
      TestLabel "fold" testFoldAggregatesValues,
      TestLabel "map" testMapTransformsValues,
      TestLabel "prefixBehavior" testPrefixBehavior
    ]

prop_monoidLeftIdentity :: StringDict -> Bool
prop_monoidLeftIdentity (StringDict dict) =
  let result = mempty <> dict
   in result == dict

prop_monoidRightIdentity :: StringDict -> Bool
prop_monoidRightIdentity (StringDict dict) =
  let result = dict <> mempty
   in result == dict

prop_monoidAssociativity :: StringDict -> StringDict -> StringDict -> Bool
prop_monoidAssociativity (StringDict a) (StringDict b) (StringDict c) =
  (a <> b) <> c == a <> (b <> c)

prop_insertThenLookup :: String -> Int -> StringDict -> Bool
prop_insertThenLookup key val (StringDict dict) =
  let dict' = insertDict key val dict
   in lookupDict key dict' == Just val

prop_deleteThenLookup :: String -> StringDict -> Bool
prop_deleteThenLookup key (StringDict dict) =
  let dict' = deleteDict key dict
   in isNothing (lookupDict key dict')

prop_filterMatchesList :: Fun (String, Int) Bool -> StringDict -> Bool
prop_filterMatchesList (Fun _ predicate) (StringDict dict) =
  let filtered = filterDict (curry predicate) dict
      expected = fromListDict (filter predicate (toListDict dict))
   in filtered == expected

prop_mapMatchesList :: Fun Int Int -> StringDict -> Bool
prop_mapMatchesList (Fun _ f) (StringDict dict) =
  let mapped = fmap f dict
      expected = fromListDict (map (second f) (toListDict dict))
   in mapped == expected

prop_foldlConsistent :: StringDict -> Bool
prop_foldlConsistent (StringDict dict) =
  let result = foldlWithKey (\acc k v -> (k, v) : acc) [] dict
      expected = reverse (toListDict dict)
   in length result == length expected

runProperty :: Testable prop => prop -> IO ()
runProperty prop = do
  result <- quickCheckResult prop
  unless (propertySucceeded result) exitFailure
  where
    propertySucceeded Success {} = True
    propertySucceeded _ = False

main :: IO ()
main = do
  putStrLn "Running unit tests..."
  resultCounts <- runTestTT unitTests
  unless (errors resultCounts == 0 && failures resultCounts == 0) exitFailure
  
  putStrLn "\nRunning property-based tests..."
  putStrLn "Testing monoid left identity..."
  runProperty prop_monoidLeftIdentity
  putStrLn "Testing monoid right identity..."
  runProperty prop_monoidRightIdentity
  putStrLn "Testing monoid associativity..."
  runProperty prop_monoidAssociativity
  putStrLn "Testing insert then lookup..."
  runProperty prop_insertThenLookup
  putStrLn "Testing delete then lookup..."
  runProperty prop_deleteThenLookup
  putStrLn "Testing filter matches list..."
  runProperty prop_filterMatchesList
  putStrLn "Testing map matches list..."
  runProperty prop_mapMatchesList
  putStrLn "Testing foldl consistency..."
  runProperty prop_foldlConsistent
  
  putStrLn "\nAll tests passed!"