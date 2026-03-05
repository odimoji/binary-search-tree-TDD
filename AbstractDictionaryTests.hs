module AbstractDictionaryTests where

import Prelude hiding (lookup, insert, delete, contains, size, isEmpty, clear, toList)
import AbstractDictionary
import qualified Test.QuickCheck as QC
import GHC.IO.Handle (hDuplicateTo, hDuplicate)
import System.IO (stdout, withFile, IOMode(..))



prop_insertLookup :: Int -> String -> Bool
prop_insertLookup k v =
    lookup k (insert k v empty) == Just v

prop_insertOverwrite :: Int -> String -> String -> Bool
prop_insertOverwrite k v1 v2 =
    AbstractDictionary.lookup k (insert k v2 (insert k v1 empty)) == Just v2

prop_insertDelete :: Int -> String -> Bool
prop_insertDelete k v =
    AbstractDictionary.lookup k (delete k (insert k v empty)) == Nothing

prop_insertContains :: Int -> String -> Bool
prop_insertContains k v =
    contains k (insert k v empty) == True

prop_deleteRemovesKey :: Int -> String -> Bool
prop_deleteRemovesKey k v =
    lookup k (delete k (insert k v empty)) == Nothing

prop_sizeAfterInsertions :: Int -> Int -> Int -> Bool
prop_sizeAfterInsertions k1 k2 k3 =
    let dict = insert k3 "v3" (insert k2 "v2" (insert k1 "v1" empty))
        uniqueKeys = length (removeDuplicates [k1, k2, k3])
    in size dict == uniqueKeys

removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates = foldr (\x seen -> if x `elem` seen then seen else x : seen) []

prop_isEmpty :: Bool
prop_isEmpty = isEmpty empty == True && isEmpty (insert 1 "one" empty) == False

prop_clear :: Int -> String -> Bool
prop_clear k v =
    isEmpty (clear (insert k v empty)) == True

prop_toList :: Bool
prop_toList =
  toList (insert 2 "two" (insert 1 "one" empty))
    == "1: one\n2: two\n"




main :: IO ()
main = do
     withFile "testOutput.txt" WriteMode $ \handle -> do
        hDuplicateTo handle stdout
        putStrLn "Running property: insert then lookup"
        QC.quickCheck prop_insertLookup

        putStrLn "Running property: insert overwrites existing key"
        QC.quickCheck prop_insertOverwrite

        putStrLn "Running property: delete after insert should make lookup return Nothing"
        QC.quickCheck prop_insertDelete

        putStrLn "Running property: contains after insert"
        QC.quickCheck prop_insertContains

        putStrLn "Running property: delete removes key"
        QC.quickCheck prop_deleteRemovesKey

        QC.quickCheck prop_sizeAfterInsertions

        putStrLn "Running property: isEmpty correctness"
        QC.quickCheck prop_isEmpty

        putStrLn "Property: clearing a dictionary results in an empty one"
        QC.quickCheck prop_clear

        putStrLn "Property: listing inserted dictionary entries"
        QC.quickCheck prop_toList



