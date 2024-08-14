-- Phantom Type

data Expr a where
    Val :: Int -> Expr a
    Add :: Expr a -> Expr a -> Expr a


-- Smart constructors
-- https://wiki.haskell.org/Smart_constructors

val :: Int -> Expr Int
val n = Val n

add :: Expr Int -> Expr Int -> Expr Int
add e1 e2 = Add e1 e2

-- El observador ahora hace las veces de función de interpretación.
-- Desempaquetamos los Int que están adentro de los constructores.

eval :: Expr a -> a
eval (Val n) = n -- No pasa el typechecker.
