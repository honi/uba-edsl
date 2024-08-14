-- Shallow Embedding

type Expr = Int

val :: Int -> Expr
val n = n

add :: Expr -> Expr -> Expr
add e1 e2 = e1 + e2

eval :: Expr -> Int
eval e = e
