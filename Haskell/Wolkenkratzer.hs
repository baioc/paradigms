-- Puzzle explanation at https://www.janko.at/Raetsel/Wolkenkratzer/

import Matrix


type Board = Matrix Int
type Constraints = ([Int], [Int], [Int], [Int]) -- (Up, Left, Bot, Right)

check :: Board -> Int -> Bool
check table n = distinctRows brd
                && distinctCols brd
                && respects brd cnstrs
    where brd = submatrix table (1,1) (n,n);
          cnstrs = ((init . tail) (row table 0),
                    (init . tail) (column table 0),
                    (init . tail) (row table (n + 1)),
                    (init . tail) (column table (n + 1)));

distinctRows :: Board -> Bool
distinctRows brd = all (\i -> distinct (row brd i)) [0..m-1]
    where (m,_) = dimension brd

distinctCols :: Board -> Bool
distinctCols brd = all (\j -> distinct (column brd j)) [0..n-1]
    where (_,n) = dimension brd

respects :: Board -> Constraints -> Bool
respects brd (u,l,b,r) =
    all (\(r,j) -> r <= 0 || skyscrapers (column brd j) == r) (zip u [0..]) &&
    all (\(r,i) -> r <= 0 || skyscrapers (row brd i) == r) (zip l [0..]) &&
    all (\(r,j) -> r <= 0 || skyscrapers (reverse (column brd j)) == r) (zip b [0..]) &&
    all (\(r,i) -> r <= 0 || skyscrapers (reverse (row brd i)) == r) (zip r [0..])

skyscrapers :: [Int] -> Int
skyscrapers (x:xs) = let (k, _) = foldl countPov (1, x) xs in k where
    countPov :: (Int, Int) -> Int -> (Int, Int)
    countPov (count, max) curr | curr > max = (count+1, curr)
                               | otherwise = (count, max)


distinct :: Eq t => [t] -> Bool
distinct []     = True
distinct (x:xs) = not (elem x xs) && distinct xs

combine :: t -> [t] -> [[t]]
combine x []     = [[x]]
combine x (y:ys) = (x:y:ys) : map (y:) (combine x ys)

permutations :: [t] -> [[t]]
permutations []     = [[]]
permutations (x:xs) = flatten (map (combine x) (permutations xs))


main = do
    let n = 5;
    let table = Matrix [[0, {-|-} 5, 3, 4, 0, 0, {-|-} 0],
                        {--------------------------------}
                        [4, {-|-} 1, 3, 2, 4, 5, {-|-} 1],
                        [3, {-|-} 2, 4, 3, 5, 1, {-|-} 0],
                        [2, {-|-} 3, 5, 4, 1, 2, {-|-} 0],
                        [2, {-|-} 4, 1, 5, 2, 3, {-|-} 0],
                        [0, {-|-} 5, 2, 1, 3, 4, {-|-} 0],
                        {--------------------------------}
                        [0, {-|-} 0, 0, 0, 2, 2, {-|-} 0]]
    print (check table n)
