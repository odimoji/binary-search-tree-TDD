module BstTests where

import Test.HUnit
import Bst
import Test.HUnit.Text (runTestText, putTextToShowS)

main :: IO ()
main = do
    
    testOutput <- runTestForOutputText allTests
    writeFile "testOutput.txt" testOutput
    putStrLn testOutput
    return ()

runTestForOutputText :: Test -> IO String
runTestForOutputText theTest = do
    (_, testOutputAccumulator) <- runTestText putTextToShowS theTest
    let testOutput = testOutputAccumulator ""
    return testOutput


allTests :: Test
allTests = TestList [
    TestCase (assertEqual "Insert into empty tree"
        (InternalNode 5 "five" Leaf Leaf)
        (insert 5 "five" Leaf)
    ),

    TestCase (assertEqual "Insert smaller key into InternalNode"
    (InternalNode 10 "ten" (InternalNode 5 "five" Leaf Leaf) Leaf)
    (insert 5 "five" (InternalNode 10 "ten" Leaf Leaf))
    ),

    TestCase (assertEqual "Insert larger key into InternalNode"
    (InternalNode 10 "ten" Leaf (InternalNode 15 "fifteen" Leaf Leaf))
    (insert 15 "fifteen" (InternalNode 10 "ten" Leaf Leaf))
    ),

    TestCase (assertEqual "Insert duplicate key into InternalNode"
    (InternalNode 10 "updated-ten" Leaf Leaf)
    (insert 10 "updated-ten" (InternalNode 10 "original-ten" Leaf Leaf))
    ),

    TestCase (assertEqual "Lookup in empty tree returns Nothing"
    Nothing
    (lookupBST 5 (Leaf :: BST Int String)) -- Refactored for parametric polymorphism
    ),

    TestCase (assertEqual "Lookup key in single-node tree returns Just value"
    (Just "five")
    (lookupBST 5 (InternalNode 5 "five" Leaf Leaf))
    ),

    TestCase (assertEqual "Lookup smaller key searches left subtree"
    (Just "four")
    (lookupBST 4 (InternalNode 5 "five" (InternalNode 4 "four" Leaf Leaf) Leaf))
    ),

    TestCase (assertEqual "Lookup larger key searches right subtree"
    (Just "six")
    (lookupBST 6 (InternalNode 5 "five" Leaf (InternalNode 6 "six" Leaf Leaf)))
    ),

    TestCase (assertEqual "Lookup missing key returns Nothing"
    Nothing
    (lookupBST 8 (InternalNode 5 "five" Leaf (InternalNode 6 "six" Leaf Leaf)))
    ),

    TestCase (assertEqual "Display entries of empty tree"
    "" 
    (displayEntriesBST (Leaf :: BST Int String)) -- Refactored for parametric polymorphism
    ),

    TestCase (assertEqual "Display single node tree"
    "5: five\n"
    (displayEntriesBST (InternalNode 5 "five" Leaf Leaf))
    ),

    TestCase (assertEqual "Display tree with left child only"
    "2: two\n5: five\n"
    (displayEntriesBST (InternalNode 5 "five" (InternalNode 2 "two" Leaf Leaf) Leaf))
    ),

    TestCase (assertEqual "Display tree with right child only"
    "5: five\n8: eight\n"
    (displayEntriesBST (InternalNode 5 "five" Leaf (InternalNode 8 "eight" Leaf Leaf)))
    ),

    TestCase (assertEqual "Display tree with both left and right children"
    "2: two\n5: five\n8: eight\n"
    (displayEntriesBST (InternalNode 5 "five" (InternalNode 2 "two" Leaf Leaf) (InternalNode 8 "eight" Leaf Leaf)))
    ),

    TestCase (assertEqual "Display empty tree"
    ""
    (displayTreeBST (Leaf :: BST Int String))
    ), -- Refactored for parametric polymorphism

    TestCase (assertEqual "Display single node tree"
    "5: five\n"
    (displayTreeBST (InternalNode 5 "five" Leaf Leaf))
    ),

    TestCase (assertEqual "Display node with only left child"
    "5: five\n  3: three\n"
    (displayTreeBST (InternalNode 5 "five" (InternalNode 3 "three" Leaf Leaf) Leaf))
    ),

    TestCase (assertEqual "Display node with only right child"
    "5: five\n  7: seven\n"
    (displayTreeBST (InternalNode 5 "five" Leaf (InternalNode 7 "seven" Leaf Leaf)))
    ),

    TestCase (assertEqual "Display node with both left and right children"
    "5: five\n  3: three\n  7: seven\n"
    (displayTreeBST (InternalNode 5 "five" (InternalNode 3 "three" Leaf Leaf) (InternalNode 7 "seven" Leaf Leaf)))
    ),

    TestCase (
    assertEqual "Deleting from an empty tree (Leaf) should return Leaf"
    (Leaf :: BST Int String)
    (delete 42 (Leaf :: BST Int String))
    ), --Refactored for Parametric Polymorphism

    TestCase ( assertEqual "Deleting the root node (key 5)"
    Leaf
    (delete 5 (InternalNode 5 "five" Leaf Leaf))
    ),

    TestCase ( assertEqual "Deleting the left child (key 3)"
    (InternalNode 5 "five" Leaf (InternalNode 7 "seven" Leaf Leaf))
    (delete 3 (InternalNode 5 "five" (InternalNode 3 "three" Leaf Leaf) (InternalNode 7 "seven" Leaf Leaf)))
    ),

    TestCase (
    assertEqual "Deleting the right child (key 7)"
    (InternalNode 5 "five" (InternalNode 3 "three" Leaf Leaf) Leaf)
    (delete 7 (InternalNode 5 "five" (InternalNode 3 "three" Leaf Leaf) (InternalNode 7 "seven" Leaf Leaf)))
    ),

    TestCase (
    assertEqual "Deleting node (key 7) with a left child"
    (InternalNode 5 "five" (InternalNode 3 "three" Leaf Leaf) (InternalNode 6 "six" Leaf Leaf))
    (delete 7 (InternalNode 5 "five" (InternalNode 3 "three" Leaf Leaf) (InternalNode 7 "seven" (InternalNode 6 "six" Leaf Leaf) Leaf)))
    ),

    TestCase (
    assertEqual "Deleting node (key 3) with a right child"
    (InternalNode 5 "five" (InternalNode 4 "four" Leaf Leaf) (InternalNode 7 "seven" Leaf Leaf))
    (delete 3 (InternalNode 5 "five" (InternalNode 3 "three" Leaf (InternalNode 4 "four" Leaf Leaf)) (InternalNode 7 "seven" Leaf Leaf)))
    ),

    TestCase (
    assertEqual "Deleting node (key 5) with both left and right children"
    (InternalNode 6 "six" (InternalNode 3 "three" Leaf Leaf) (InternalNode 7 "seven" Leaf Leaf))
    (delete 5 (InternalNode 5 "five" (InternalNode 3 "three" Leaf Leaf) (InternalNode 6 "six" Leaf (InternalNode 7 "seven" Leaf Leaf))))
    ),

    TestCase (
    assertEqual "rotateLeft on an empty tree should return Leaf"
    (Leaf :: BST Int String)
    (rotateLeft (Leaf :: BST Int String))
    ),

    TestCase ( assertEqual "rotateLeft on a valid tree"
    (InternalNode 7 "seven" 
        (InternalNode 5 "five" 
            (InternalNode 3 "three" Leaf Leaf)
            (InternalNode 6 "six" Leaf Leaf)
        )
        (InternalNode 8 "eight" Leaf Leaf)
    )
    (rotateLeft (InternalNode 5 "five" 
        (InternalNode 3 "three" Leaf Leaf) 
        (InternalNode 7 "seven" 
            (InternalNode 6 "six" Leaf Leaf) 
            (InternalNode 8 "eight" Leaf Leaf)
        )
    ))
    ),

    TestCase ( assertEqual "rotateLeft on invalid tree (no right child) should return tree unchanged"
    (InternalNode 5 "five" (InternalNode 3 "three" Leaf Leaf) Leaf)
    (rotateLeft (InternalNode 5 "five" (InternalNode 3 "three" Leaf Leaf) Leaf))
    ),

    TestCase (
    assertEqual "rotateRight on empty or invalid tree should return the tree unchanged"
    (InternalNode 5 "five" Leaf (InternalNode 7 "seven" Leaf Leaf))
    (rotateRight (InternalNode 5 "five" Leaf (InternalNode 7 "seven" Leaf Leaf)))
    ),

    TestCase (
    assertEqual "rotateRight on a valid tree with left child"
    (InternalNode 3 "three" Leaf (InternalNode 5 "five" Leaf (InternalNode 7 "seven" Leaf Leaf)))
    (rotateRight (InternalNode 5 "five" (InternalNode 3 "three" Leaf Leaf) (InternalNode 7 "seven" Leaf Leaf)))
    ),

    TestCase (assertEqual "countIf on empty tree should return 0"
    0
    (countIf (\_ -> True) (Leaf :: BST Int String))
    ),

    TestCase (assertEqual "countIf on single-node tree with predicate returning True"
    1
    (countIf (\v -> v == "five") (InternalNode 5 "five" Leaf Leaf))
    ),

    TestCase (assertEqual "countIf on single-node tree with predicate returning False"
    0
    (countIf (\v -> v == "seven") (InternalNode 5 "five" Leaf Leaf))
    ),

    TestCase (assertEqual "countIf on tree with only left child matching"
    1
    (countIf (\v -> v == "two") 
        (InternalNode 5 "five" 
            (InternalNode 2 "two" Leaf Leaf) 
            Leaf
        )
    )
    ),

    TestCase (assertEqual "countIf on tree with only right child matching"
    1
    (countIf (\v -> v == "eight") 
        (InternalNode 5 "five" 
            Leaf 
            (InternalNode 8 "eight" Leaf Leaf)
        )
    )
    ),

    TestCase (assertEqual "countIf on tree with multiple matches"
    2
    (countIf (\v -> v == "match")
        (InternalNode 10 "ten"
            (InternalNode 5 "match" Leaf Leaf)
            (InternalNode 15 "match" Leaf Leaf)
        )
    )
    )










  ]
