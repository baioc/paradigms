import Prelude hiding (gcd, lcm, map, filter)


avaliar :: Float -> Float -> Float -> String
avaliar p1 p2 p3 = if m < 6.0 then "reprovado" else "aprovado"
                    where m = (p1 + p2 + p3) / 3

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib(n-1) + fib(n-2)

gcd :: Int -> Int -> Int
gcd a b | b == 0 = a
        | otherwise = gcd b (a `mod` b)

lcm :: Int -> Int -> Int
lcm a b = (abs a*b) `div` (gcd a b)

map :: (x -> y) -> [x] -> [y]
map func [] = []
map func (a:b) = func a : map func b

filter :: (t -> Bool) -> [t] -> [t]
filter pred [] = []
filter pred (a:b) = let rest = filter pred b in
                        if pred a then a : rest else rest


main = do
    putStr "Hello, World!\n"
    -- line <- getLine
    -- let val = (read line :: Int)
    -- val <- readLn :: IO Int
    -- print (val)
    print(fib(16))
    print (lcm 4 (gcd 24 (gcd 54 9)))
