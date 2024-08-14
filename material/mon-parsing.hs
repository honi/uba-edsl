
import Control.Monad
import GHC.Base hiding ((<|>))

---------------------------------------------------

-- | Parser type 

newtype Parser a = P {runP :: String -> [(a,String)]}

instance Functor Parser where
  fmap f p = P $ \cs -> [(f a,cs') | (a,cs') <- runP p cs]

instance Applicative Parser where
  pure a =  P (\cs -> [(a,cs)])
  -- (<*>) ::  Parser (a -> b) -> Parser a -> Parser b
  (P p) <*> (P q) = P $ \cs -> [ (f a, cs'')  |  (f , cs')   <- p cs
                                              ,  (a , cs'')  <- q cs']

instance Monad Parser where
  return a    = P $ \cs -> [(a,cs)]
  (P p) >>= f = P $ \cs -> concat [runP (f a) cs' | (a,cs') <- p cs]

-- | Parsers primitivos

pFail :: Parser a
pFail = P $ \cs -> []

(<|>) :: Parser a -> Parser a -> Parser a
(P p) <|> (P q) = P $ \cs -> case p cs ++ q cs of
                              []     -> []
                              (x:xs) -> [x]

item :: Parser Char
item = P $ \cs -> case cs of
                    ""     -> []
                    (c:cs) -> [(c,cs)]

pSat :: (Char -> Bool) -> Parser Char
pSat p = do c <- item
            if p c then return c
                   else pFail

pSym :: Char -> Parser Char
pSym c = pSat (== c)

-- | p* (many) cero o más veces p

pList :: Parser a -> Parser [a]
pList p = do a <- p
             as <- pList p
             return (a:as)
          <|>
          return []

-- | p+ (some) una o más veces p

pListP :: Parser a -> Parser [a]
pListP p = do a <- p
              as <- pList p
              return (a:as)

-- | parsear una lista de dígitos

-- parsear un dígito y retornar el entero correspondiente
digit :: Parser Int
digit = do c <- pSat isDigit
           return (ord c - ord '0')

isDigit c = (c >= '0') && (c <= '9')

-- parsear una lista de dígitos no vacía
digits :: Parser [Int]
digits = pListP digit

-- sumar la lista de dígitos

sumDigits :: Parser Int
sumDigits = do ds <- digits
               return (sum ds)

-- suma de dígitos es par?
isEvenSumDs :: Parser Bool
isEvenSumDs = do n <- sumDigits
                 return (even n)
-- even n = n `mod` 2 == 0                

-- | reconocer un literal entero (reconoce una lista de dígitos 
-- | y retorna el entero que denota)

number :: Parser Int
number = do d <- digit
            number' d

number' :: Int -> Parser Int
number' n = do d <- digit
               number' (n*10 + d)
            <|>
            return n

-- | otra forma (en 2 pasos)

numberFoldl = do ds <- digits
                 return (foldl accum 0 ds)
  where
    accum n d = n*10 + d  

-- | parsear una expresión 

data Expr = Val Int 
          | Add Expr Expr
     deriving Show

-- versión que entra en loop (recursión a la izquierda)

badexpr :: Parser Expr
badexpr  = do e1 <- badexpr
              pSym '+'
              e2 <- badexpr
              return (Add e1 e2)
           <|>
           do n <- number
              return (Val n)

-- versión correcta

expr  :: Parser Expr
expr  =  do n <- number
            pSym '+'
            e <- expr
            return (Add (Val n) e)
            <|>
            do n <- number
               return (Val n)

-- | parsing y evaluación

evalExpr = do e <- expr
              return (eval e)

eval (Val n) = n
eval (Add e e') = eval e + eval e'

-- | versión fusionada

evalExpr_fus  :: Parser Int
evalExpr_fus  = do n <- number
                   pSym '+'
                   e <- evalExpr_fus
                   return (n + e)
                  <|>
                  number  

-- | expresiones con paréntesis

expr_par :: Parser Expr
expr_par = do e1 <- sumando
              pSym '+'
              e2 <- expr_par
              return (Add e1 e2)
           <|> 
           sumando
               
sumando :: Parser Expr
sumando = do pSym '('
             e <- expr_par 
             pSym ')'
             return e
             <|>
             do n <- number
                return (Val n)             

evalExpr_par = do e <- expr_par
                  return (eval e)

-- | nano XML

data Xml = Tag Char [Xml]
 deriving Show

xml_par :: Parser Xml
xml_par = do -- se parsea el tag de apertura
             pSym '<'
             name <- item
             pSym '>'
             -- se parsea la lista de XMLs internos
             xmls <- pList xml_par
             -- se parsea el tag de cierre
             pSym '<'
             pSym '/'
             pSym name  -- se utiliza el nombre del tag de apertura
             pSym '>'
             return $ Tag name xmls

             
