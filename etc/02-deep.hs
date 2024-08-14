-- Deep Embedding

data Expr where
    Val :: Int -> Expr
    Add :: Expr -> Expr -> Expr


-- Smart constructors
-- https://wiki.haskell.org/Smart_constructors

val :: Int -> Expr
val n = Val n

add :: Expr -> Expr -> Expr
add e1 e2 = Add e1 e2

-- El observador ahora hace las veces de función de interpretación.
-- Desempaquetamos los Int que están adentro de los constructores.
-- Estoy solo funciona bien si todas las expresiones son de tipo Int.

eval :: Expr -> Int
eval (Val n) = n
eval (Add e1 e2) = eval e1 + eval e2
