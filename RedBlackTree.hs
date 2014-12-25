data Tree a = Leaf | Red a (Tree a) (Tree a) | Black a (Tree a) (Tree a)
  deriving Eq


valueOf (Red v _ _) = v
valueOf (Black v _ _) = v

right Leaf = Leaf
right (Red _ _ r) = r
right (Black _ _ r) = r

left Leaf = Leaf
left (Red _ l _) = l
left (Black _ l _) = l

isRed (Red _ _ _) = True
isRed _ = False

isBlack = not . isRed


rotateLeft :: (Tree a) -> (Tree a)
rotateLeft Leaf = Leaf
rotateLeft (Red   v a (Red v' b c)) = Red   v' (Red v a b) c
rotateLeft (Black v a (Red v' b c)) = Black v' (Red v a b) c
rotateLeft tree = tree


rotateRight Leaf = Leaf
rotateRight (Red   v (Red v' a b) c) = Red   v' a (Red v b c)
rotateRight (Black v (Red v' a b) c) = Black v' a (Red v b c)
rotateRight tree = tree


makeBlack tree = Black (valueOf tree) (left tree) (right tree)

makeRed tree = Red (valueOf tree) (left tree) (right tree)


flipColors Leaf = Leaf
flipColors tree = Red (valueOf tree) (makeBlack $ left tree) (makeBlack $ right tree)


fixTree Leaf = Leaf
fixTree tree
  | (isRed r) && (isBlack l) = rotateLeft tree
  | (isRed l) && (isRed $ left l) = rotateRight tree
  | (isRed l) && (isRed r) = flipColors tree
  | otherwise = tree
  where l = left tree
        r = right tree


insert value = makeBlack . insert' value

insert' value Leaf = Red value Leaf Leaf
insert' value tree @ (Red v l r)
  | value == v = tree
  | value <  v = fixTree $ Red v (go l) r
  | value >  v = fixTree $ Red v l (go r)
  where go = insert' value
insert' value tree @ (Black v l r)
  | value == v = tree
  | value <  v = fixTree $ Black v (go l) r
  | value >  v = fixTree $ Black v l (go r)
  where go = insert' value


insertAll (x:xs) tree = insert x (insertAll xs tree)
insertAll [] tree = tree


{-printInsert :: (Show a) => [a] -> Tree a -> IO ()-}
printInsert [] _ = return ()
printInsert (x:xs) tree = do
    let tree' = insert x tree
    putStrLn $ (show x) ++ ": " ++ (show tree')
    printInsert xs tree'


instance (Show a) => Show (Tree a) where
  show Leaf = "#"
  show tree = 
    "(" ++ color ++ show v ++ " -> " ++ show l ++ " " ++ show r ++ ")"
    where color = if isRed tree then "R " else "B "
          v = valueOf tree
          l = left tree
          r = right tree


main = do
  let nodes = reverse [100, 50, 25, 35, 75, 60, 65, 150]
  {-printInsert nodes Leaf-}
  let tree = insertAll nodes Leaf
  print tree

