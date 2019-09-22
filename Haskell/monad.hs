-- data Maybe a = Nothing | Just a
--     deriving (Eq, Ord, Read, Show)

-- instance Monad Maybe where
--     Nothing  >>= f = Nothing
--     (Just x) >>= f = f x
--     return         = Just

data Expr t = Val t
            | Div (Expr t) (Expr t)
            | Mul (Expr t) (Expr t)
            | Add (Expr t) (Expr t)
            | Sub (Expr t) (Expr t)
            | Sqrt (Expr t)
    deriving (Show, Read)

div_ :: Float -> Float -> Maybe Float
div_ n d | d == 0    = Nothing
         | otherwise = Just (n / d)

mul_ :: Float -> Float -> Maybe Float
mul_ m n = Just (m * n)

add_ :: Float -> Float -> Maybe Float
add_ a b = Just (a + b)

sub_ :: Float -> Float -> Maybe Float
sub_ a b = Just (a - b)

sqrt_ :: Float -> Maybe Float
sqrt_ x = if x < 0 then Nothing else Just (fixedPoint (averageDamp (\y -> x / y)) 1) where
    averageDamp f = (\x -> ((f x) + x) / 2)
    fixedPoint f xo = iter xo (f x) where
        iter old new = if (abs (old - new)) < 1/10000 then new else iter new (f new)

evalcase :: Expr Float -> Maybe Float
evalcase (Val x)   = Just x
evalcase (Div x y) = case (evalcase x) of
                          Nothing -> Nothing
                          Just a  -> case (evalcase y) of
                                          Nothing -> Nothing
                                          Just b  -> div_ a b
evalcase (Mul x y) = case (evalcase x) of
                          Nothing -> Nothing
                          Just a  -> case (evalcase y) of
                                          Nothing -> Nothing
                                          Just b  -> mul_ a b
evalcase (Add x y) = case (evalcase x) of
                      Nothing -> Nothing
                      Just a  -> case (evalcase y) of
                                      Nothing -> Nothing
                                      Just b  -> add_ a b
evalcase (Sub x y) = case (evalcase x) of
                      Nothing -> Nothing
                      Just a  -> case (evalcase y) of
                                      Nothing -> Nothing
                                      Just b  -> sub_ a b
evalcase (Sqrt x) = case (evalcase x) of
                         Nothing -> Nothing
                         Just a  -> sqrt_ a

evalmonad :: Expr Float -> Maybe Float
evalmonad (Val x)   = return x
evalmonad (Div x y) = evalmonad x >>=
                        (\a -> evalmonad y >>=
                            \b -> div_ a b)
evalmonad (Mul x y) = evalmonad x >>=
                        (\a -> evalmonad y >>=
                            \b -> mul_ a b)
evalmonad (Add x y) = evalmonad x >>=
                        (\a -> evalmonad y >>=
                            \b -> add_ a b)
evalmonad (Sub x y) = evalmonad x >>=
                        (\a -> evalmonad y >>=
                            \b -> sub_ a b)
evalmonad (Sqrt x)  = evalmonad x >>= (\a -> sqrt_ a)

evaldo :: Expr Float -> Maybe Float
evaldo (Val x)   = return x
evaldo (Div x y) = do
    a <- evaldo x
    b <- evaldo y
    div_ a b
evaldo (Mul x y) = do
    a <- evaldo x
    b <- evaldo y
    mul_ a b
evaldo (Add x y) = do
    a <- evaldo x
    b <- evaldo y
    add_ a b
evaldo (Sub x y) = do
    a <- evaldo x
    b <- evaldo y
    sub_ a b
evaldo (Sqrt x)  = do
    a <- evaldo x
    sqrt_ a

putChars :: String -> Int -> IO ()
putChars _ 0     = return ()
putChars [] _    = return ()
putChars (h:t) n = do { putChar h; putChars t (n-1); }

echoChar :: IO ()
echoChar = getChar >>= putChar

main = do
    print (evalcase (Sqrt (Val (- 5.0))))
    print (evalcase (Mul (Div (Add (Val 28)
                                   (Val 2))
                              (Sub (Val 6)
                                   (Val 1)))
                         (Sqrt (Val 9))))
    print (evalmonad (Sqrt (Val (- 5.0))))
    print (evalmonad (Mul (Div (Add (Val 28)
                                    (Val 2))
                               (Sub (Val 6)
                                    (Val 1)))
                          (Sqrt (Val 9))))
    print (evaldo (Sqrt (Val (- 5.0))))
    print (evaldo (Mul (Div (Add (Val 28)
                                 (Val 2))
                            (Sub (Val 6)
                                 (Val 1)))
                       (Sqrt (Val 9))))

    putChars "123456789" 5
    putStr "\n> "
    echoChar
    putChar '\n'
