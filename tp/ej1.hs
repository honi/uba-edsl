{-# OPTIONS_GHC -Wno-star-is-type #-}

{-
Shallow embedding bien tipado usando enfoque tagless-final.

Especificamos el EDSL como funciones en el typeclass `Expr e`. Éstas funciones
operan con el tipo `e` que tiene kind * -> * permitiendo especializar el
primer * del tipo `e` para codificar en el tipo de la expresión cuál es el tipo
primitivo de Haskell "embebido" adentro de la expresión (Int o Bool).
-}

import qualified Prelude    -- Para poder usar Prelude.not.
import Prelude hiding (not, and, or)

class Expr (e :: * -> *) where
    val :: Int -> e Int
    eq  :: e Int -> e Int -> e Bool
    lt  :: e Int -> e Int -> e Bool
    not :: e Bool -> e Bool
    and :: e Bool -> e Bool -> e Bool
    or  :: e Bool -> e Bool -> e Bool

{-
Para dar la semántica del EDSL, creamos un nuevo tipo concreto y definimos su
instancia para el typeclass `Expr`.
-}

data Eval e = E e
    deriving Show

{-
Los tipos de las funciones de la clase `Expr` nos permiten apoyarnos en el
typechecker de Haskell para garantizar que los términos se construyen con
valores bien tipados (`val` solo puede recibir un `E Int`, mientras que `not`
solo puede recibir `E Bool`).
-}

instance Expr Eval where
    val x = E x
    eq (E x) (E y) = E (x == y)
    lt (E x) (E y) = E (x < y)
    not (E p) = E (Prelude.not p)
    and (E p) (E q) = E (p && q)
    or (E p) (E q) = E (p || q)

{-
El shallow embedding define la semántica como una instancia de la clase `Expr`.
Esto significa que si queremos dar otra interpretación al EDSL, por ejemplo
para un pretty print de la expresón, necesitamos definir un nuevo tipo `Print e`
para luego definir una nueva instancia de la clase `Expr`.

A diferencia del deep embedding, las expresiones del EDSL no construyen un AST.
En cambio, cada término del EDSL mapea a la función correspondiente definida en
la instancia de la clase `Expr` para el tipo asignado a la expresión.

Esto resulta interesante ya que al construir una expresión aún no hemos hecho
ningún trabajo, ninguna computación. Recién al darle un tipo concreto a la
expresión y forzar su evaluación es que reducimos la expresión a su valor.

En el caso del tipo `Eval e`, el resultado de la evaluación es el resultado de
la expresión en sí misma (`e` puede resultar ser Int o Bool). En el caso del
tipo `Print e`, el resultado es siempre un String que representa a la expresión
en sí misma. En este caso el tipo `e` de `Print e` es un phantom type.
-}

data Print e = P String
    deriving Show

instance Expr Print where
    val n = P (show n)
    eq (P x) (P y) = P ("(" ++ (x ++ " == " ++ y) ++ ")")
    lt (P x) (P y) = P ("(" ++ (x ++ " < " ++ y) ++ ")")
    not (P p) = P ("~" ++ p)
    and (P p) (P q) = P ("(" ++ (p ++ " && " ++ q) ++ ")")
    or (P p) (P q) = P ("(" ++ (p ++ " || " ++ q) ++ ")")

{-
Definimos algunas expresiones para probar.
-}

e1 :: Expr e => e Int
e1 = val 42

e2 :: Expr e => e Bool
e2 = eq (val 1) (val 42)

e3 :: Expr e => e Bool
e3 = lt (val 1) (val 42)

e4 :: Expr e => e Bool
e4 = not e2

e5 :: Expr e => e Bool
e5 = and e2 e3

e6 :: Expr e => e Bool
e6 = or e2 e3

{-
Para reducir estas expresiones necesitamos forzarles el tipo apropiado según
la interpretación que queremos. `Eval e` para evaluar, `Print e` para imprimir.
-}

evalInt :: Eval Int -> Int
evalInt (E n) = n

evalBool :: Eval Bool -> Bool
evalBool (E b) = b

printInt :: Print Int -> String
printInt (P n) = n

printBool :: Print Bool -> String
printBool (P b) = b

test :: Bool
test = evalInt  e1 == 42
    && evalBool e2 == False
    && evalBool e3 == True
    && evalBool e4 == True
    && evalBool e5 == False
    && evalBool e6 == True

{-
Ejemplos del typechecker en acción. Estas expresiones no tipan.
-}

-- noTipa1 = e1 :: Eval Bool
-- noTipa2 = e2 :: Eval Int
-- noTipa3 = e3 :: Print Int
-- noTipa4 = e4 :: Print Int
