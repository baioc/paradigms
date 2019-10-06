-- Puzzle explanation at https://www.janko.at/Raetsel/Wolkenkratzer/

import Matrix


type Board = Matrix Int -- x <= 0 => blank
type Constraints = ([Int], [Int], [Int], [Int]) -- (Up, Left, Bot, Right)

-- check if a given board satisfies rules and constraints
check :: Constraints -> Board -> Bool
check cnstrs board = distinctRows board && distinctCols board && respects board cnstrs

distinctRows :: Board -> Bool
distinctRows board = let (m,_) = dimension board in
    all (\i -> distinct (row board i)) [0..m-1]

distinctCols :: Board -> Bool
distinctCols board = let (_,n) = dimension board in
    all (\j -> distinct (column board j)) [0..n-1]

respects :: Board -> Constraints -> Bool
respects board (u,l,b,r) =
       all (\(r,j) -> r <= 0 || skyscrapers (column board j) == r) (zip u [0..])
    && all (\(r,i) -> r <= 0 || skyscrapers (row board i) == r) (zip l [0..])
    && all (\(r,j) -> r <= 0 || skyscrapers (reverse (column board j)) == r) (zip b [0..])
    && all (\(r,i) -> r <= 0 || skyscrapers (reverse (row board i)) == r) (zip r [0..])

-- count visible skyscrapers
skyscrapers :: [Int] -> Int
skyscrapers (x:xs) = let (k, _) = foldl countPov (1,x) xs in k where
    countPov :: (Int, Int) -> Int -> (Int, Int)
    countPov (count, max) curr | curr > max = (count+1, curr)
                               | otherwise = (count, max)


-- check uniqueness in list
distinct :: Eq t => [t] -> Bool
distinct []     = True
distinct (x:xs) = not (elem x xs) && distinct xs

-- inserts a value in a list at every possible position
combine :: t -> [t] -> [[t]]
combine x []     = [[x]]
combine x (y:ys) = (x:y:ys) : map (y:) (combine x ys)

-- generate all permutations of a list
permutations :: [t] -> [[t]]
permutations []     = [[]]
permutations (x:xs) = (permutations xs) >>= (combine x)

-- find first list element that satisfies a predicate
find :: (t -> Bool) -> [t] -> Maybe t
find pred [] = Nothing
find pred (x:xs) = if pred x then Just x else find pred xs


-- filter all rows which have the nth element equal to a reference row's
filterPos :: Eq t => [t] -> [[t]] -> [[t]]
filterPos ref rest = aux ref rest 0 where
    aux [] ys _     = ys
    aux (x:xs) ys n = aux xs (filter (\y -> y !! n /= x) ys) (n+1)

-- generate an association list for all possible ranged-line permutation pairings
possibleLines :: Int  -> [([Int],[[Int]])]
possibleLines n = let perms = permutations [1..n] in
    perms >>= (\row -> map (\arr -> (row, (filterPos row arr))) [perms])

-- generate all possible arrays with n lines from allowed line pairings
possibleArrays :: Int -> [Int] -> [([Int],[[Int]])] -> [[[Int]]]
possibleArrays 2 row allowed = maybe [] makeArray (lookup row allowed) where
    makeArray lines = map (\line -> [row, line]) lines
possibleArrays m row allowed = maybe [] (makeArray (restrict allowed row)) (lookup row allowed) where
    restrict pls line = map (\(r, ps) -> (r, filterPos line ps)) pls
    makeArray pls [] = []
    makeArray pls ys = ys >>= \y -> (map (row:) (possibleArrays (m-1) y pls))

-- all n*n ranged matrix permutations with no repeated values in rows or columns
possibilities :: Int -> [[[Int]]]
possibilities n = let pls = possibleLines n in
    pls >>= (\(row, _) -> possibleArrays n row pls)


main = do
    -- https://www.janko.at/Raetsel/Wolkenkratzer/072.a.htm
    -- let n = 4; -- 0.472s
    -- let table = [[0, {-|-} 4, 1, 0, 0, {-|-} 0],
    --              {-----------------------------}
    --              [0, {-|-} 1, 4, 2, 3, {-|-} 0],
    --              [3, {-|-} 2, 1, 3, 4, {-|-} 0],
    --              [2, {-|-} 3, 2, 4, 1, {-|-} 0],
    --              [0, {-|-} 4, 3, 1, 2, {-|-} 3],
    --              {-----------------------------}
    --              [0, {-|-} 1, 2, 2, 0, {-|-} 0]]

    -- https://www.janko.at/Raetsel/Wolkenkratzer/078.a.htm
    let n = 5; -- 0.708s
    let table = [[0, {-|-} 5, 3, 4, 0, 0, {-|-} 0],
                 {--------------------------------}
                 [4, {-|-} 1, 3, 2, 4, 5, {-|-} 1],
                 [3, {-|-} 2, 4, 3, 5, 1, {-|-} 0],
                 [2, {-|-} 3, 5, 4, 1, 2, {-|-} 0],
                 [2, {-|-} 4, 1, 5, 2, 3, {-|-} 0],
                 [0, {-|-} 5, 2, 1, 3, 4, {-|-} 0],
                 {--------------------------------}
                 [0, {-|-} 0, 0, 0, 2, 2, {-|-} 0]]

    -- https://www.janko.at/Raetsel/Wolkenkratzer/003.a.htm
    -- let n = 6; -- 3h53m32.37s
    -- let table = [[0, {-|-} 4, 1, 2, 2, 3, 2, {-|-} 0],
    --              {--------------------------------}
    --              [2, {-|-} 2, 6, 3, 4, 1, 5, {-|-} 2],
    --              [3, {-|-} 4, 5, 6, 3, 2, 1, {-|-} 4],
    --              [3, {-|-} 3, 1, 5, 2, 6, 4, {-|-} 2],
    --              [4, {-|-} 1, 2, 4, 6, 5, 3, {-|-} 3],
    --              [2, {-|-} 5, 3, 2, 1, 4, 6, {-|-} 1],
    --              [1, {-|-} 6, 4, 1, 5, 3, 2, {-|-} 4],
    --              {--------------------------------}
    --              [0, {-|-} 1, 3, 5, 2, 4, 2, {-|-} 0]]

    let restricts = (constraints table)
    let solutions = find (check restricts)
                         (map (Matrix) (possibilities n))
    case (solutions) of
        Nothing  -> print "Impossible!"
        Just brd -> print brd

constraints :: [[Int]] -> Constraints
constraints tab = let n = length tab; table = Matrix tab in
    ((init . tail) (row table 0),
     (init . tail) (column table 0),
     (init . tail) (row table (n-1)),
     (init . tail) (column table (n-1)))
