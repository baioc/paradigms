myTree :: (BinaryTree Int)
myTree = Node 52 (Node 32 (Node 12 Null Null)
                          (Node 32 Null Null))
                 (Node 56 (Node 55 Null Null)
                          (Node 64 Null Null))

myIndexTree :: (BinaryTree Int)
myIndexTree = (Node 0 (Node 1 Null Null)
                      (Node 2 Null Null))

main = do
    putStr ((show (list myTree)) ++ "\n")
    print (average myTree :: Float)
    print ((==) myTree myIndexTree)
    print (height myIndexTree)


data BinaryTree t = Null | Node t (BinaryTree t) (BinaryTree t) -- can't be done with newtype

instance (Eq t) => Eq (BinaryTree t) where
    (Node va la ra) == (Node vb lb rb) = va == vb && la == lb && ra == rb
    Null            == Null            = True
    _               == _               = False

average :: (Integral i, Fractional f) => (BinaryTree i) -> f
average Null = 0
average tree = let (sum, len) = avg tree 0 0 in (fromIntegral sum) / len where
    avg Null acc n = (acc, n)
    avg (Node v l r) acc n = (accLeft + v + accRight, nLeft + 1 + lRight) where
        (accLeft, nLeft) = avg l 0 0
        (accRight, lRight) = avg r 0 0

list :: (BinaryTree t) -> [t]
list Null = []
list (Node val left right) =  [val] ++ list left ++ list right

height :: (BinaryTree t) -> Int
height (Node v Null Null) = 0
height Null = -1
height (Node v l r) = 1 + max (height l) (height r)


type Pair t u = (t, u) -- type aliasing
data PairData t u = PairData { pairFst :: t, pairSnd :: u } -- new data type wrapper
-- newtype PairType t u = PairType { pairTypeFst :: t, pairTypeSnd :: u } -- more than one constructor field not allowed
newtype PairType t u = PairType (t, u) -- new static type, isomorphic to Pair
