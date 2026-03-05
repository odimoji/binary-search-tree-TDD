module AbstractDictionary (
    Dict,
    insert,
    lookup,
    empty, 
    contains,
    delete, 
    isEmpty,
    size, 
    clear, 
    toList
) where

import Prelude hiding (lookup, insert, delete, contains, size, isEmpty, clear, toList)
import qualified Bst

newtype Dict k v = Dict (Bst.BST k v)

-- Create an empty dictionary
empty :: Dict k v
empty = Dict Bst.Leaf

insert :: Ord k => k -> v -> Dict k v -> Dict k v
insert k v (Dict tree) = Dict (Bst.insert k v tree)

lookup :: Ord k => k -> Dict k v -> Maybe v
lookup k (Dict tree) = Bst.lookupBST k tree

delete :: Ord k => k -> Dict k v -> Dict k v
delete k (Dict tree) = Dict (Bst.delete k tree)

contains :: Ord k => k -> Dict k v -> Bool
contains k dict = case lookup k dict of
    Just _  -> True
    Nothing -> False

size :: Dict k v -> Int
size (Dict tree) = countNodes tree
  where
    countNodes :: Bst.BST k v -> Int
    countNodes Bst.Leaf = 0
    countNodes (Bst.InternalNode _ _ l r) = 1 + countNodes l + countNodes r

isEmpty :: Dict k v -> Bool
isEmpty (Dict tree) = countNodes tree == 0
  where
    countNodes :: Bst.BST k v -> Int
    countNodes Bst.Leaf = 0
    countNodes (Bst.InternalNode _ _ l r) = 1 + countNodes l + countNodes r

clear :: Dict k v -> Dict k v
clear _ = empty


toList :: Show k => Dict k String -> String
toList (Dict tree) = Bst.displayEntriesBST tree



