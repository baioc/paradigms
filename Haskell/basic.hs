import Prelude hiding (gcd, lcm)


hasPassed :: (Float, Float, Float) -> String
hasPassed (p1, p2, p3) = if m < 6.0 then "No" else "Yes"
                            where m = (p1 + p2 + p3) / 3

fib :: Int -> Integer -- Int: fixnum, Integer: flonum
fib 0 = 0
fib 1 = 1
fib n = fib(n-1) + fib(n-2)

gcd :: Integral t => t -> t -> t
gcd a b | b == 0 = a
        | otherwise = gcd b (a `mod` b)

lcm :: Integral t => t -> t -> t
lcm a b = (abs a*b) `div` (gcd a b)


main = do
    putStr "Hello, World!\n"
    -- line <- getLine
    -- let val = (read line :: Integer)
    -- print (val)
    -- val <- readLn :: IO Integer
    -- print (val)
    print(fib(16))
    print (lcm 4 (gcd 24 (gcd 54 9)))
