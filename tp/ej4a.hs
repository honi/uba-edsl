{-
Ciencias de la Computación - FCEN - UBA
ECI 2024

Jonathan Bekenstein
jbekenstein@dc.uba.ar
LU 348/11
-}

{-# OPTIONS_GHC -Wno-star-is-type #-}
module Ej4a where

import qualified Prelude    -- Para poder usar Prelude.not.
import Prelude hiding (not, and, or, lookup)
import qualified Data.Map.Strict as Map
import Data.Maybe

-- Usamos un Map como contexto (ambiente) de variables.
type Context = Map.Map String Bool

-- Asumimos que la variable siempre está definida en el contexto.
lookup :: Context -> String -> Bool
lookup ctx s = fromJust (Map.lookup s ctx)

createContext = Map.fromList

class Expr (e :: * -> *) where
    val :: Int -> e Int
    var :: Context -> String -> e Bool
    eq  :: e Int -> e Int -> e Bool
    lt  :: e Int -> e Int -> e Bool
    not :: e Bool -> e Bool
    and :: e Bool -> e Bool -> e Bool
    or  :: e Bool -> e Bool -> e Bool

data Eval e = E e
    deriving Show

instance Expr Eval where
    val x = E x
    var ctx s = E (lookup ctx s)
    eq (E x) (E y) = E (x == y)
    lt (E x) (E y) = E (x < y)
    not (E p) = E (Prelude.not p)
    and (E p) (E q) = E (p && q)
    or (E p) (E q) = E (p || q)

evalInt :: Eval Int -> Int
evalInt (E n) = n

evalBool :: Eval Bool -> Bool
evalBool (E b) = b

data Print e = P String
    deriving (Show, Eq)

instance Expr Print where
    val n = P (show n)
    var ctx s = P s
    eq (P x) (P y) = P ("(" ++ (x ++ " == " ++ y) ++ ")")
    lt (P x) (P y) = P ("(" ++ (x ++ " < " ++ y) ++ ")")
    not (P p) = P ("~" ++ p)
    and (P p) (P q) = P ("(" ++ (p ++ " && " ++ q) ++ ")")
    or (P p) (P q) = P ("(" ++ (p ++ " || " ++ q) ++ ")")

{-
Queremos poder reutilizar la misma expresión con diferentes contextos. Una forma
podría ser así: definimos la expresión en función de un contexto dado. Quien
quiera evaluar esta expresión tiene que pasar un contexto instanciado
adecuadamente, es decir, el contexto deber tener definidas todas las variables
que aparecen en la expresión (sino el lookup tira un error).
-}

expr :: Expr e => Context -> e Bool
expr ctx = and (eq (val 42) (val 42)) (var ctx "x")

{-
Evaluamos la expresión con 2 contextos distintos y vemos que el resultado de la
evaluación es diferente en cada caso.
-}

evalTrue = evalBool $ expr $ createContext [("x", True)]
evalFalse = evalBool $ expr $ createContext [("x", False)]

test :: Bool
test = evalTrue  == True
    && evalFalse == False
    && expr (createContext []) == (P "((42 == 42) && x)")
