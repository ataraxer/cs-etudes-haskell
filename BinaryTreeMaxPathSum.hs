data Tree = Node Int Tree Tree | Leaf
data Result = Result Int Int
  deriving Show

result (Result value _) = value

leaf value = Node value Leaf Leaf


maxPath :: Tree -> Result
maxPath Leaf = Result 0 0
maxPath (Node value Leaf Leaf) = Result value value
maxPath (Node value left right) = Result bestValue nodeValue
  where (Result bestLeft  leftValue)  = maxPath left
        (Result bestRight rightValue) = maxPath right
        bestValue = maximum [rootSum, bestLeft, bestRight]
        nodeValue = maximum [leftValue + value, rightValue + value, value]
        rootSum = leftValue + rightValue + value


tree1 = Node (-15)
  (Node 5
    (Node (-8)
      (leaf 2)
      (leaf 6))
    (leaf 1))
  (Node 6
    (leaf 3)
    (Node 9
      Leaf
      (Node 0
        (leaf 4)
        (Node (-1)
          (leaf 10)
          Leaf))))


tree2 = Node 1
  (Node (-2) (leaf 4) (leaf 5))
  (Node   3  (leaf 6) (Node 7 Leaf (leaf (-8))))


tree3 = Node (-1) (leaf (-2)) (leaf (-3))


main = print $ fmap (result . maxPath) [tree1, tree2, tree3]
