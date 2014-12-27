data Color = Red | Black deriving Eq
data Tree a = Leaf | Node Color a (Tree a) (Tree a)
  deriving Eq


color Leaf = Black
color (Node c _ _ _) = c

valueOf (Node _ v _ _) = v

left Leaf = Leaf
left (Node _ _ l _) = l

right Leaf = Leaf
right (Node _ _ _ r) = r

isRed   = (== Red) . color
isBlack = not . isRed

makeRed   (Node c v l r) = Node Red   v l r
makeBlack (Node c v l r) = Node Black v l r


rotateLeft (Node c v x (Node Red v' y z)) = Node c v' (Node Red v x y) z
rotateLeft tree = tree


rotateRight (Node c v (Node Red v' x y) z) = Node c v' x (Node Red v y z)
rotateRight tree = tree


flipColors Leaf = Leaf
flipColors (Node _ v l r) = Node Red v (makeBlack l) (makeBlack r)


fixTree Leaf = Leaf
fixTree tree
  | (isRed r) && (isBlack l) = rotateLeft tree
  | (isRed l) && (isRed $ left l) = rotateRight tree
  | (isRed l) && (isRed r) = flipColors tree
  | otherwise = tree
  where l = left tree
        r = right tree


insert value = makeBlack . insert' value
  where insert' value Leaf = Node Red value Leaf Leaf
        insert' value tree @ (Node c v l r)
          | value == v = tree
          | value <  v = fixTree $ Node c v (go l) r
          | value >  v = fixTree $ Node c v l (go r)
          where go = insert' value


insertAll (x:xs) = insertAll xs . insert x
insertAll [] = id


{-printInsert :: (Show a) => [a] -> Tree a -> IO ()-}
printInsert [] _ = return ()
printInsert (x:xs) tree = do
    let tree' = insert x tree
    putStrLn $ (show x) ++ ": " ++ (show tree')
    printInsert xs tree'


instance (Show a) => Show (Tree a) where
  show Leaf = "#"
  show tree @ (Node _ v l r) =
    "(" ++ color ++ (show v) ++ " -> " ++ (show l) ++ " " ++ (show r) ++ ")"
    where color = if isRed tree then "R " else "B "


main = do
  let nodes = reverse [100, 50, 25, 35, 75, 60, 65, 150]
  {-printInsert nodes Leaf-}
  let tree = insertAll nodes Leaf
  print tree

