data Tree a = Node a (Tree a) (Tree a) | Leaf deriving Eq


valueOf (Node v _ _) = v
right (Node _  _ r) = r
left (Node _ l _) = l


{-find :: a -> Tree a -> Maybe (Tree a)-}
find value Leaf = Nothing
find value tree @ (Node v l r)
  | value == v = Just tree
  | value <  v = find value l
  | value >  v = find value r


{-exists :: a -> Tree a -> Bool-}
exists value tree = found /= Nothing
  where found = find value tree


{-insert :: a -> Tree a -> Tree a-}
insert value Leaf = Node value Leaf Leaf
insert value tree @ (Node v l r)
  | value == v = tree
  | value <  v = Node v (insert value l) r
  | value >  v = Node v l (insert value r)


{-minNode :: Tree a -> Tree a-}
minNode Leaf = Leaf
minNode tree @ (Node _ Leaf _) = tree
minNode tree = minNode $ left tree


{-maxNode :: Tree a -> Tree a-}
maxNode Leaf = Leaf
maxNode tree @ (Node _ _ Leaf) = tree
maxNode tree = maxNode $ right tree


{-successor :: Tree a -> Tree a-}
successor Leaf = Leaf
successor tree = minNode $ right tree


{-removeMin :: Tree a -> Tree a-}
removeMin Leaf = Leaf
removeMin (Node v (Node _ Leaf l') r) = Node v l' r
removeMin tree = removeMin $ left tree


{-remove :: a -> Tree a -> Tree a-}
remove value Leaf = Leaf
remove value tree @ (Node v l r)
  | value <  v = Node v (remove value l) r
  | value >  v = Node v l (remove value r)
  | value == v = Node v' l r'
  where s = successor tree
        v' = valueOf s
        r' = removeMin r


insertAll (x:xs) tree = insert x (insertAll xs tree)
insertAll [] tree = tree

preOrder Leaf = []
preOrder (Node v l r) = [v] ++ preOrder l ++ preOrder r

inOrder Leaf = []
inOrder (Node v l r) = inOrder l ++ [v] ++ inOrder r

postOrder Leaf = []
postOrder (Node v l r) = postOrder l ++ postOrder r ++ [v]


bst Leaf = []
bst tree @ (Node v l r) = go l [v] [r]
  where go _ result [] = reverse result
        go Leaf vs (q:qs) = go q vs qs
        go (Node v l r) vs (q:qs) = go q (v:vs) (qs ++ [l, r])


instance (Show a) => Show (Tree a) where
  show Leaf = "#"
  show tree @ (Node v l r) =
    show v ++ " " ++  "(" ++ (show l) ++ " " ++ (show r) ++ ")"


main = do
  let nodes = reverse [100, 50, 25, 35, 75, 60, 65, 150]
  let tree = insertAll nodes Leaf
  print tree
  print $ remove 50 tree
  let nodes' = reverse ['F', 'B', 'A', 'D', 'C', 'E', 'G', 'I', 'H']
  let tree' = insertAll nodes' Leaf
  print $ preOrder tree'
  print $ inOrder tree'
  print $ postOrder tree'
  print $ bst tree'

