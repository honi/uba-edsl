{-# LANGUAGE GADTs #-}

{-
Deep embedding bien tipado utilizando GADTs.

Los GADTs nos permiten especializar la variable de tipo `a` en cada constructor
del tipo Expr a. De esta forma restringimos qué valores podemos pasarle a los
constructores (por ejemplo Val solo acepta un Int), pero también codificamos en
el tipo de la expresión qué tipo de valor tenemos "adentro".

Es decir, si la expresión contiene un valor Int va a ser del tipo Expr Int.
Análogamente si contiene un valor Bool va a ser del tipo Expr Bool. Estos
constructores no permiten especializar la variable de tipo `a` en ningún otro
tipo que no sea Int o Bool.

De esta forma nos apoyamos en el typechecker propio de Haskell para validar
durante compilación que las expresiones estén bien formadas. Además nos permite
escribir el evaluador de forma muy práctica ya que al pattern matchear con cada
constructor inferimos correctamente el tipo del valor que está "adentro" (un Int
o un Bool) y podemos realizar operaciones sobre esos tipos en el host language.
-}

data Expr a where
    Val :: Int -> Expr Int
    Eq  :: Expr Int -> Expr Int -> Expr Bool
    Lt  :: Expr Int -> Expr Int -> Expr Bool
    Not :: Expr Bool -> Expr Bool
    And :: Expr Bool -> Expr Bool -> Expr Bool
    Or  :: Expr Bool -> Expr Bool -> Expr Bool

deriving instance Show (Expr a)

-- Evaluador de expresiones.
eval :: Expr a -> a
eval (Val n) = n
eval (Eq e1 e2) = eval e1 == eval e2
eval (Lt e1 e2) = eval e1 < eval e2
eval (Not e) = not (eval e)
eval (And e1 e2) = eval e1 && eval e2
eval (Or e1 e2) = eval e1 || eval e2

-- Pretty printer de una expresión.
printExpr :: Expr a -> String
printExpr (Val n) = show n
printExpr (Eq e1 e2) = printExpr e1 ++ " == " ++ printExpr e2
printExpr (Lt e1 e2) = printExpr e1 ++ " < " ++ printExpr e2
printExpr (Not e) = "~(" ++ printExpr e ++ ")"
printExpr (And e1 e2) = printExpr e1 ++ " && " ++ printExpr e2
printExpr (Or e1 e2) = printExpr e1 ++ " || " ++ printExpr e2

-- Algunas expresiones de ejemplo para probar.
e1 = Val 42 :: Expr Int
e2 = Eq (Val 1) (Val 42) :: Expr Bool
e3 = Lt (Val 1) (Val 42) :: Expr Bool
e4 = Not e2 :: Expr Bool
e5 = And e2 e3 :: Expr Bool
e6 = Or e2 e3 :: Expr Bool

test :: Bool
test = eval e1 == 42
    && eval e2 == False
    && eval e3 == True
    && eval e4 == True
    && eval e5 == False
    && eval e6 == True
