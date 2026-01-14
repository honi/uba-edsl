{-
Ciencias de la Computación - FCEN - UBA
ECI 2024

Jonathan Bekenstein
jbekenstein@dc.uba.ar
LU 348/11
-}

{-# LANGUAGE GADTs #-}
module Ej4b where

import Prelude hiding (lookup)
import qualified Data.Map.Strict as Map
import Data.Maybe

-- Usamos un Map como contexto (ambiente) de variables.
type Context = Map.Map String Bool

-- Asumimos que la variable siempre está definida en el contexto.
lookup :: Context -> String -> Bool
lookup ctx s = fromJust (Map.lookup s ctx)

createContext = Map.fromList

data Expr a where
    Val :: Int -> Expr Int
    Var :: String -> Expr Bool
    Eq  :: Expr Int -> Expr Int -> Expr Bool
    Lt  :: Expr Int -> Expr Int -> Expr Bool
    Not :: Expr Bool -> Expr Bool
    And :: Expr Bool -> Expr Bool -> Expr Bool
    Or  :: Expr Bool -> Expr Bool -> Expr Bool

deriving instance Show (Expr a)

printExpr :: Expr a -> String
printExpr (Val n) = show n
printExpr (Var s) = s
printExpr (Eq e1 e2) = "(" ++ printExpr e1 ++ " == " ++ printExpr e2 ++ ")"
printExpr (Lt e1 e2) = "(" ++ printExpr e1 ++ " < " ++ printExpr e2 ++ ")"
printExpr (Not e) = "~" ++ printExpr e
printExpr (And e1 e2) = "(" ++ printExpr e1 ++ " && " ++ printExpr e2 ++ ")"
printExpr (Or e1 e2) = "(" ++ printExpr e1 ++ " || " ++ printExpr e2 ++ ")"

{-
Con deep embedding obtenemos un AST de la expresión. Esto permite mantener
desacoplada la representación de la expresión respecto al entorno de evaluación.
-}

eval :: Context -> Expr a -> a
eval ctx (Val n) = n
eval ctx (Var s) = lookup ctx s
eval ctx (Eq e1 e2) = eval ctx e1 == eval ctx e2
eval ctx (Lt e1 e2) = eval ctx e1 < eval ctx e2
eval ctx (Not e) = not (eval ctx e)
eval ctx (And e1 e2) = eval ctx e1 && eval ctx e2
eval ctx (Or e1 e2) = eval ctx e1 || eval ctx e2

expr = And (Lt (Val 1) (Val 42)) (Var "x") :: Expr Bool

evalTrue = eval (createContext [("x", True)]) expr
evalFalse = eval (createContext [("x", False)]) expr

test :: Bool
test = evalTrue  == True
    && evalFalse == False
    && printExpr expr == "((1 < 42) && x)"
