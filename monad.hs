data Expr = Val Int | Div Expr Expr

--eval :: Expr -> Int
--eval (Val n) = n
--eval (Div x y) = eval x `div` eval y

safediv :: Int -> Int -> Maybe Int
safediv _ 0 = Nothing
safediv n m = Just (n `div` m)

eval :: Expr -> Maybe Int
eval (Val n) = Just n
eval (Div x y) = case eval x of
  Nothing -> Nothing
  Just n -> case eval y of
    Nothing -> Nothing
    Just m -> safediv n m


--evalA (Val n) = id
evalA (Div x y) = pure safediv <*> 8 <*> 4 -- <*> evalA y
--(>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
--mx >>= f = case mx of
--             Nothing -> Nothing
--             Just x -> f x

evalM :: Expr -> Maybe Int
evalM (Val n) = Just n
evalM (Div x y) = evalM x >>= \d ->
                                evalM y >>= \m ->
                                              safediv d m

evalM1 :: Expr -> Maybe Int
evalM1 (Val n) = Just n
evalM1 (Div x y) = do n <- eval x
                      m <- eval y
                      safediv n m

evalM2 :: Expr -> Maybe Int
evalM2 (Val n) = Just n
evalM2 (Div x y) = do n <- evalM2 x
                      m <- evalM2 y
                      safediv n m

pairs :: [Int] -> [Int] -> [(Int)]
pairs xs ys = do x <- xs
                 y <- ys
                 return (x+y)
