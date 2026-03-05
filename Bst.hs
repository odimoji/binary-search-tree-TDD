module Bst where



data BST k v = Leaf
             | InternalNode k v (BST k v) (BST k v)
             deriving (Eq, Show)


insert :: Ord k => k -> v -> BST k v -> BST k v
insert k v Leaf = InternalNode k v Leaf Leaf
insert k v (InternalNode key val left right)
    | k == key  = InternalNode key v left right
    | k < key   = InternalNode key val (insert k v left) right
    | otherwise = InternalNode key val left (insert k v right)

lookupBST :: Ord k => k -> BST k v -> Maybe v
lookupBST _ Leaf = Nothing
lookupBST k (InternalNode key val left right)
    | k == key  = Just val
    | k < key   = lookupBST k left
    | otherwise = lookupBST k right


displayEntriesBST :: Show k => BST k String -> String
displayEntriesBST Leaf = ""
displayEntriesBST (InternalNode key value left right) =
    displayEntriesBST left ++ show key ++ ": " ++ value ++ "\n" ++ displayEntriesBST right




displayTreeBST :: Show k => BST k String -> String
displayTreeBST Leaf = ""
displayTreeBST (InternalNode key value left right) =
    show key ++ ": " ++ value ++ "\n" ++ indent (displayTreeBST left ++ displayTreeBST right)



indent :: String -> String
indent = unlines . map ("  " ++) . lines

delete :: Ord k => k -> BST k v -> BST k v
delete _ Leaf = Leaf
delete k (InternalNode key value left right)
    | k < key   = InternalNode key value (delete k left) right
    | k > key   = InternalNode key value left (delete k right)
    | otherwise =
        case (left, right) of
            (Leaf, Leaf) -> Leaf
            (_, Leaf)    -> left
            (Leaf, _)    -> right
            (_, _)       ->
                let (succKey, succVal) = findMin right
                in InternalNode succKey succVal left (delete succKey right)

                


findMin :: BST k v -> (k, v)
findMin (InternalNode key val Leaf _) = (key, val)
findMin (InternalNode _ _ left _) = findMin left
findMin Leaf = error "findMin called on empty tree"

rotateLeft :: BST k v -> BST k v
rotateLeft (InternalNode key val left (InternalNode rKey rVal rLeft rRight)) =
    InternalNode rKey rVal (InternalNode key val left rLeft) rRight
rotateLeft t = t

rotateRight :: BST k v -> BST k v
rotateRight (InternalNode key val (InternalNode lKey lVal lLeft lRight) right) =
    InternalNode lKey lVal lLeft (InternalNode key val lRight right)
rotateRight t = t

countIf :: (v -> Bool) -> BST k v -> Int
countIf _ Leaf = 0
countIf p (InternalNode _ val left right) =
    let c = if p val then 1 else 0
        l = countIf p left
        r = countIf p right
    in c + l + r


