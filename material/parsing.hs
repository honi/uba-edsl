{-# LANGUAGE GADTs, KindSignatures #-}

module Parsing where

import Prelude hiding ((<*>),(<$>),(<*),(*>),(<$))
import Data.Char

type Parser s a = [s] -> [(a, [s])]

pFail  :: Parser s a
pFail  = \cs -> []

pSucceed    :: a -> Parser s a
pSucceed a  =  \cs -> [(a,cs)]

pSym    :: Eq s => s -> Parser s s
pSym s  = \cs ->  case cs of
                    []         ->  []
                    (c : cs')  ->  if  c == s
                                       then [(c,cs')]
                                       else []

infixl 3 <|> 
infixl 4 <*> 
infixl 4 <* 
infixl 4 *> 
infixl 4 <$> 
infixl 4 <$ 

(<|>) ::  Parser s a -> Parser s a -> Parser s a
p <|> q = \cs -> p cs ++ q cs

(<*>) ::  Parser s (a -> b) -> Parser s a -> Parser s b
(p <*> q) cs = [ (f a, cs'')  |  (f , cs')   <- p cs
                              ,  (a , cs'')  <- q cs']

f <$> p  = pSucceed f <*> p

p <* q   = (\ x _ -> x) <$> p <*> q

p *> q   = (\ _ y -> y) <$> p <*> q

a <$ q   = pSucceed a <* q

p `opt` v = p <|> pSucceed v

pSat    :: (s -> Bool) -> Parser s s
pSat p  = \cs ->  case cs of
                   []         -> []
                   (c : cs')  -> if  p c
                                     then [(c,cs')]
                                     else []

-- p* (many)
pList :: Parser s a -> Parser s [a]
pList p =  (:) <$> p <*> pList p
           <|>
           pSucceed []

-- p+ (some)
pListP :: Parser s a -> Parser s [a]
pListP p =  (:) <$> p <*> pList p

-- | Ejemplos

pA2B = pSucceed (\_ -> 'B') <*> pSym 'A'

pAB = pSucceed (,) <*> pSym 'A' <*> pSym 'B'

pListAB = pList pAB

pParens p = pSym '(' *> p <* pSym ')'

pDigit = pSat isDigit
-- isDigit c = (c >= '0') && (c <= '9')

pDigits = pListP pDigit

digit = d2int <$> pDigit
  where d2int d = ord(d) - ord('0')
        
digits = pListP digit
-- otra forma
-- digits = map d2int <$> pDigits

number = horner <$> digits

horner = foldl (\n d -> n*10 + d) 0

data Expr = Val Int | Add Expr Expr 
  deriving Show 

eval (Val n) = n 
eval (Add e1 e2) = eval e1 + eval e2  

-- expr  :: Parser Char Expr
expr  = (\n _ e -> Add (Val n) e) <$> number <*> pSym '+' <*> expr
        <|>
        Val <$> number

evalExpr = (\n _ e -> n+e) <$> number <*> pSym '+' <*> evalExpr
        <|>
        number


