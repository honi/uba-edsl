{-# OPTIONS_GHC -Wno-star-is-type #-}

import qualified Prelude    -- Para poder usar Prelude.not.
import Prelude hiding (not, and, or)
import qualified Data.Map.Strict as Ctx
import Data.Maybe

-- Usamos un Map como contexto (ambiente) de variables.
type Context = Ctx.Map String Bool

class Expr (e :: * -> *) where
    val :: Int -> e Int
    var :: String -> Context -> e Bool
    eq  :: e Int -> e Int -> e Bool
    lt  :: e Int -> e Int -> e Bool
    not :: e Bool -> e Bool
    and :: e Bool -> e Bool -> e Bool
    or  :: e Bool -> e Bool -> e Bool

data Eval e = E e
    deriving Show

instance Expr Eval where
    val x = E x
    -- Asumimos que la variable siempre está definida en el contexto.
    var s ctx = E (fromJust (Ctx.lookup s ctx))
    eq (E x) (E y) = E (x == y)
    lt (E x) (E y) = E (x < y)
    not (E p) = E (Prelude.not p)
    and (E p) (E q) = E (p && q)
    or (E p) (E q) = E (p || q)

evalInt :: Eval Int -> Int
evalInt (E n) = n

evalBool :: Eval Bool -> Bool
evalBool (E b) = b

{-
Queremos poder reutilizar la misma expresión con diferentes contextos. Una forma
podría ser así: definimos la expresión en función de un contexto dado. Quien
quiera evaluar esta expresión tiene que pasar un contexto instanciado
adecuadamente, es decir, el contexto deber tener definidas todas las variables
que aparecen en la expresión (sino el lookup tira un error).
-}

expr :: Expr e => Context -> e Bool
expr ctx = and (eq (val 42) (val 42)) (var "x" ctx)

{-
Evaluamos la expresión con 2 contextos distintos y vemos que el resultado de la
evaluación es diferente en cada caso.
-}

evalTrue = evalBool $ expr $ Ctx.fromList [("x", True)]
evalFalse = evalBool $ expr $ Ctx.fromList [("x", False)]

test :: Bool
test = evalTrue  == True
    && evalFalse == False
