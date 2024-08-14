{-# LANGUAGE GADTs #-}

-- GADTs (Generalised Algebraic DataTypes)

data Expr a where
    Val :: Int -> Expr Int
    Add :: Expr Int -> Expr Int -> Expr Int


eval :: Expr a -> a
eval (Val n) = n
eval (Add e1 e2) = eval e1 + eval e2
