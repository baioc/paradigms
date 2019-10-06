module Matrix (
    Matrix(..),
    matrix,
    fill,
    submatrix,
    mapx,
    zipx,
    toList,
    fromList,
    dimension,
    row,
    column,
    diagonal,
    get,
) where


newtype Matrix t = Matrix [[t]]

-- pretty-printing
instance Show t => Show (Matrix t) where
    show (Matrix []) = "[]"
    show (Matrix m)  = "[" ++ auxShow m ++ "]" where
        auxShow [row] = show row
        auxShow (h:t) = show h ++ ",\n " ++ auxShow t

-- fmap :: (a -> b) -> Matrix a -> Matrix b
instance Functor Matrix where
    fmap _ (Matrix [])   = Matrix []
    fmap f (Matrix rows) = Matrix (map (map f) rows)


-- 2D index-based constructor
matrix :: Int -> Int -> ((Int,Int) -> t) -> Matrix t
matrix m n g = mapx g (fill m n 0)

-- make a matrix filled with some value
fill :: Int -> Int -> t -> Matrix t
fill m n x = Matrix (map (\_ -> map (\_ -> x) [1..n]) [1..m])


-- index-based sub-block extraction
submatrix :: Matrix t -> (Int,Int) -> (Int,Int) -> Matrix t
submatrix (Matrix rows) (0,0) (m,n) = Matrix (map (take n) (take m rows))
submatrix (Matrix rows) (0,j) dim = submatrix (Matrix (map tail rows)) (0,j-1) dim
submatrix (Matrix (y:ys)) (i,j) dim = submatrix (Matrix ys) (i-1, j) dim

-- index-based fmap
mapx :: ((Int,Int) -> b) -> Matrix a -> Matrix b
mapx _ (Matrix []) = Matrix []
mapx f m = fmap (\(_, pos) -> f pos) (zipx m)

-- pair values to their indexes
zipx :: Matrix t -> Matrix (t, (Int,Int))
zipx (Matrix []) = Matrix []
zipx (Matrix rows) = Matrix (map (\(row,i) -> map (\(x,j) -> (x, (i,j)))
                                                  (zip row [0..]))
                                 (zip rows [0..]))

anyx :: (t -> Bool) -> Matrix t -> Bool
anyx _ (Matrix []) = False
anyx pred (Matrix (row:rest)) = any pred row || anyx pred (Matrix rest)


-- flattening conversion
toList :: Matrix t -> [t]
toList (Matrix m) = m >>= id

-- folding conversion
fromList :: Int -> Int -> [t] -> Matrix t
fromList _ _ []  = Matrix []
fromList 0 _ _   = Matrix []
fromList _ 0 _   = Matrix []
fromList m n lst = Matrix (row:sub) where
    (row, rest)  = splitAt n lst
    (Matrix sub) = fromList (m - 1) n rest


-- supposing all rows have the same size
dimension :: Matrix t -> (Int, Int)
dimension (Matrix (row:rest)) = (length rest + 1, length row)

-- ith line
row :: Matrix t -> Int -> [t]
row (Matrix m) i = m !! i

-- jth column as a list
column :: Matrix t -> Int -> [t]
column (Matrix m) j = map (!! j) m

-- list main diagonal
diagonal :: Matrix t -> [t]
diagonal m = map (\(x, _) -> x)
                 (filter (\(_, (i,j)) -> i == j)
                         (toList (zipx m)))

-- index-based element-wise random access
get :: Matrix t -> (Int,Int) -> t
get (Matrix m) (i,j) = m !! i !! j


main = do
    let t = Matrix [[1, 2, 3],
                    [4, 5, 6],
                    [7, 8, 9]]
    print t

    print (Matrix [] :: Matrix String)

    let z = fill 3 3 0
    print z

    let m = matrix 4 4 (\(i,j) -> 4*i + j)
    print m

    let n = (toList m)
    print n
    print (fromList 4 4 n)

    print (row t 0)
    print (column t 0)
    print (diagonal t)
    print (get t (0,0))

    print (submatrix t (0,0) (2,2))
