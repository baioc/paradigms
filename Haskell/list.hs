import Prelude hiding (map, filter)


map :: (x -> y) -> [x] -> [y]
map func [] = []
map func (a:b) = func a : map func b

filter :: (t -> Bool) -> [t] -> [t]
filter pred [] = []
filter pred (a:b) = let rest = filter pred b in
                        if pred a then a : rest else rest

elems :: Eq t => [t] -> [t] -> Bool
elems [] [] = True
elems [] (a:b) = False
elems (a:b) [] = False
elems (a:b) lst = elem a lst && elems b (removeFirstOf lst a)

removeFirstOf :: Eq t => [t] -> t -> [t]
removeFirstOf [] x = []
removeFirstOf (a:b) x = if a == x then b else a : removeFirstOf b x

distinct :: Eq t => [t] -> Bool
distinct []     = True
distinct (x:xs) = not (elem x xs) && distinct xs

powerSet :: [t] -> [[t]]
powerSet [] = [[]]
powerSet (first:rest) = let psRest = powerSet rest in
                            psRest ++ map (\subSet -> first : subSet) psRest


main = do
    print (map (+1) [0, 1, 2, 3])
    print (elems [0, 1, 2, 3] [0, 1, 3, 2, 0])
    print (powerSet ['a', 'b', 'c'])
    print (primes !! 999) -- 1000th prime number


sieve :: Integral t => [t] -> [t]
sieve [] = []
sieve (head:tail) = head : sieve (filter (\x -> rem x head /= 0) tail)

primes :: [Integer]
primes = sieve [2..]
