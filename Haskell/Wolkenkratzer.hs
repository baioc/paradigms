module Wolkenkratzer (solve) where

import Matrix

solve :: [[Int]] -> Int -> Maybe (Matrix Int)
solve table hi = do
    let n = (length table) - 2 -- detect board size
    let t = if hi == n then id else (\x -> if x >= n then 0 else x) -- blank space allowance
    let restricts = (constraints table) -- extract restrictions
    let prune = if hi == n -- choose wether to apply some heuristics
                then (\brd -> (match (board table) brd) && (collapse n restricts brd))
                else (match (board table)) -- or not
    find (check restricts) -- lazily build possible boards and look for a solution after pruning
         (filter prune (map (Matrix) (possibilities n t)))

main = do -- examples
    {- https://www.janko.at/Raetsel/Wolkenkratzer/072.a.htm  0.007s ~ 0.086s -}
    -- let max = 4
    -- let puzzle = [[0, {-|-} 4, 1, 0, 0, {-|-} 0],
    --               {-----------------------------}
    --               [0, {-|-} 0, 0, 0, 0, {-|-} 0],
    --               [3, {-|-} 0, 0, 0, 0, {-|-} 0],
    --               [2, {-|-} 0, 0, 0, 0, {-|-} 0],
    --               [0, {-|-} 0, 0, 0, 0, {-|-} 3],
    --               {-----------------------------}
    --               [0, {-|-} 1, 2, 2, 0, {-|-} 0]]
    {- https://www.janko.at/Raetsel/Wolkenkratzer/325.a.htm  0.057s ~ 0.187s -}
    let max = 4
    let puzzle = [[0, {-|-} 0, 3, 1, 0, 2, {-|-} 0],
                  {--------------------------------}
                  [0, {-|-} 0, 0, 0, 0, 0, {-|-} 3],
                  [0, {-|-} 0, 0, 0, 0, 0, {-|-} 1],
                  [0, {-|-} 0, 0, 0, 0, 0, {-|-} 0],
                  [0, {-|-} 0, 0, 0, 0, 0, {-|-} 4],
                  [0, {-|-} 0, 0, 0, 0, 0, {-|-} 4],
                  {--------------------------------}
                  [0, {-|-} 0, 1, 2, 2, 0, {-|-} 0]]
    {- https://www.janko.at/Raetsel/Wolkenkratzer/044.a.htm  3h19m33.64s ~ 3h53m32.37s -}
    -- let max = 6
    -- let puzzle = [[0, {-|-} 6, 5, 0, 4, 3, 0, {-|-} 0],
    --               {--------------------------------}
    --               [0, {-|-} 0, 0, 0, 0, 0, 0, {-|-} 0],
    --               [0, {-|-} 0, 0, 0, 0, 0, 0, {-|-} 1],
    --               [4, {-|-} 0, 0, 0, 0, 0, 0, {-|-} 2],
    --               [3, {-|-} 0, 0, 0, 0, 0, 0, {-|-} 0],
    --               [2, {-|-} 0, 0, 0, 0, 0, 0, {-|-} 3],
    --               [0, {-|-} 0, 0, 0, 0, 0, 0, {-|-} 0],
    --               {--------------------------------}
    --               [0, {-|-} 0, 2, 0, 2, 0, 0, {-|-} 0]]
    -- print results
    case (solve puzzle max) of
        Nothing     -> putStr "Impossible!\n"
        Just solved -> print solved


type Board = Matrix Int                         -- x <= 0 -> ignore
type Constraints = ([Int], [Int], [Int], [Int]) -- (Up, Left, Bot, Right)

-- extract constraints from table-array
constraints :: [[Int]] -> Constraints
constraints tab = let n = length tab; table = Matrix tab in
    ((init . tail) (row table 0),
     (init . tail) (column table 0),
     (init . tail) (row table (n-1)),
     (init . tail) (column table (n-1)))

-- extract board from table-array
board :: [[Int]] -> Board
board table = let n = length table; tab = Matrix table in
    submatrix tab (1, 1) (n-2, n-2)


-- check if a given board satisfies constraints, O(4n^2)
check :: Constraints -> Board -> Bool
check (u,l,b,r) brd =
       all (\(r,j) -> r <= 0 || skyscrapers (column brd j) == r) (zip u [0..])
    && all (\(r,i) -> r <= 0 || skyscrapers (row brd i) == r) (zip l [0..])
    && all (\(r,j) -> r <= 0 || skyscrapers (reverse (column brd j)) == r) (zip b [0..])
    && all (\(r,i) -> r <= 0 || skyscrapers (reverse (row brd i)) == r) (zip r [0..])

-- count visible skyscrapers, O(n)
skyscrapers :: [Int] -> Int
skyscrapers (x:xs) = let (count,_) = foldl countPov (if x > 0 then 1 else 0, x) xs in count where
    countPov (count, hi) curr | curr > hi = (count+1, curr)
                               | otherwise = (count, hi)


-- apply heuristics to prune impossible boards, only valid in the absence of parks
collapse :: Int -> Constraints -> Board -> Bool
collapse n (up,left,bot,right) board = not (any upperPrune (zip up [0..]))
                                    && not (any leftPrune (zip left [0..]))
                                    && not (any bottomPrune (zip bot [0..]))
                                    && not (any rightPrune (zip right [0..]))
    where
        upperPrune (k,j) | k <= 0 = False
                         | k == n = column board j /= [1..n]
                         | k == 1 = get board (0,j) /= n
                         | otherwise = any (\(x,d) -> x == n && d < k)
                                           (zip (column board j) [1..n])
        leftPrune (k,i) | k <= 0 = False
                        | k == n = row board i /= [1..n]
                        | k == 1 = get board (i,0) /= n
                        | otherwise = any (\(x,d) -> x == n && d < k)
                                          (zip (row board i) [1..n])
        bottomPrune (k,j) | k <= 0 = False
                          | k == n = column board j /= reverse [1..n]
                          | k == 1 = get board (n-1,j) /= n
                          | otherwise = any (\(x,d) -> x == n && d < k)
                                            (zip (column board j) (reverse [1..n]))
        rightPrune (k,i) | k <= 0 = False
                         | k == n = row board i /= reverse [1..n]
                         | k == 1 = get board (i,n-1) /= n
                         | otherwise = any (\(x,d) -> x == n && d < k)
                                           (zip (row board i) (reverse [1..n]))

-- filter boards that match given "tips" taken as constraints
match :: Board -> Board -> Bool
match tips brd = not (anyx (\(ref,pos) -> ref > 0 && get brd pos /= ref) (zipx tips))


-- find first list element that satisfies a predicate
find :: (t -> Bool) -> [t] -> Maybe t
find pred [] = Nothing
find pred (x:xs) = if pred x then Just x else find pred xs

-- inserts a value in a list at every possible position
combine :: t -> [t] -> [[t]]
combine x []     = [[x]]
combine x (y:ys) = (x:y:ys) : map (y:) (combine x ys)

-- generate all permutations of a list
permutations :: [t] -> [[t]]
permutations []     = [[]]
permutations (x:xs) = permutations xs >>= combine x


-- filter all rows which have the nth element equal to a reference row's for all n
filterPos :: Eq t => [t] -> [[t]] -> [[t]]
filterPos ref rest = aux ref rest 0 where
    aux [] ys _     = ys
    aux (x:xs) ys n = aux xs (filter (\y -> y !! n /= x) ys) (n+1)

-- generate an association list for all possible line permutation pairings
possibleLines :: Int -> (Int -> Int) -> [([Int],[[Int]])]
possibleLines n transform = let perms = map (\p -> map transform p) (permutations [1..n]) in
    perms >>= (\row -> map (\all -> (row, (filterPos row all))) [perms])

-- generate all possible arrays with n lines from allowed line pairings
possibleArrays :: Int -> [Int] -> [([Int],[[Int]])] -> [[[Int]]]
possibleArrays 2 row allowed = maybe [] makeArray (lookup row allowed) where
    makeArray lines = map (\line -> [row, line]) lines
possibleArrays m row allowed = maybe [] (makeArray (restrict allowed row)) (lookup row allowed) where
    -- edit possible lines (pls) by filtering incompatible lines
    restrict pls line = map (\(r, ps) -> (r, filterPos line ps)) pls
    -- append row after recursively building array list of size m-1
    makeArray pls [] = []
    makeArray pls ys = ys >>= \y -> map (row:) (possibleArrays (m-1) y pls)

-- all n*n chaotic permutations matrices with no repeated values in rows or columns
possibilities :: Int -> (Int -> Int) -> [[[Int]]]
possibilities n transform = let pls = possibleLines n transform in
    pls >>= \(row, _) -> possibleArrays n row pls
