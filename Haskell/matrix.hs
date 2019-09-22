module Matrix where

data Matrix t = Matrix [[t]]

instance Show t => Show (Matrix t) where
    show (Matrix []) = "[[]]"
    show (Matrix m)  = "[" ++ auxShow m ++ "]" where
        auxShow [row] = show row
        auxShow (h:t) = show h ++ ",\n " ++ auxShow t

instance Functor Matrix where
    fmap _ (Matrix [])   = Matrix []
    fmap f (Matrix rows) = Matrix (map (\row -> map f row) rows)

fill :: Int -> Int -> t -> Matrix t
fill m n x = Matrix (map (\_ -> map (\_ -> x) [1..n]) [1..m])

matrix :: Int -> Int -> ((Int,Int) -> t) -> Matrix t
matrix m n g = mapx g (fill m n 0)

mapx :: ((Int,Int) -> b) -> Matrix a -> Matrix b
mapx _ (Matrix []) = Matrix []
mapx f m = fmap (\(_, pos) -> f pos) (zipx m)

zipx :: Matrix t -> Matrix (t, (Int,Int))
zipx (Matrix []) = Matrix []
zipx (Matrix rows) = Matrix (map (\(row,i) -> map (\(x,j) -> (x, (i,j)))
                                                  (zip row [0..]))
                                 (zip rows [0..]))

flatten :: [[t]] -> [t]
flatten []     = []
flatten (x:xs) = x ++ flatten xs

toList :: Matrix t -> [t]
toList (Matrix m) = flatten m

fromList :: Int -> Int -> [t] -> Matrix t
fromList _ _ []  = Matrix []
fromList 0 _ _   = Matrix []
fromList _ 0 _   = Matrix []
fromList m n lst = Matrix (row : sub) where
    (row, rest)  = splitAt n lst
    (Matrix sub) = fromList (m - 1) n rest

dim :: Matrix t -> (Int, Int)
dim (Matrix (row:rest)) = (length rest + 1, length row)

get :: Matrix t -> (Int, Int) -> t
get (Matrix m) (i,j) = m !! i !! j

set :: Matrix t -> (Int, Int) -> t -> Matrix t
set m pos new = fmap (\(old, idx) -> if idx == pos then new else old) (zipx m)

row :: Matrix t -> Int -> [t]
row (Matrix m) i = m !! i

col :: Matrix t -> Int -> [t]
col (Matrix m) j = map (!!j) m

diag :: Matrix t -> [t]
diag m = map (\(x, _) -> x)
             (filter (\(_, (i,j)) -> i == j)
                     (toList (zipx m)))

add :: Num t => Matrix t -> Matrix t -> Matrix t
add a b = fromList m n (zipWith (+) (toList a) (toList b)) where (m, n) = dim a


main = do
    let t = Matrix [[1, 2, 3],
                    [4, 5, 6],
                    [7, 8, 9]]
    print t

    print (Matrix [] :: Matrix String)

    let z = fill 3 3 0
    print z

    print (zipx t)

    let m = matrix 4 4 (\(i,j) -> 2*i - j)
    print m

    let n = (toList m)
    print n
    print (fromList 4 4 n)

    print (add t m)

    print (get t (0,0))
    print (set t (0,0) (-1))
    print (row t 0)
    print (col t 0)
    print (diag t)
