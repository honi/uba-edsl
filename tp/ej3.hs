import Parsing
import Control.Monad

-- Análogo a pSym, pToken consume una cantidad arbitraria de carácteres.
pToken :: String -> Parser String
pToken t = mapM pSat (map (==) t)

-- Corre el parser p encerrado en paréntesis.
parenthesis :: Parser a -> Parser a
parenthesis p = do
    pSym '('
    x <- p
    pSym ')'
    return x

-- Simplifica el parseo de una expresión binaria.
binary :: (UProp -> UProp -> UProp)     -- Constructor binario de UProp.
       -> Parser UProp                  -- Parser de la izquierda.
       -> String                        -- Token del medio.
       -> Parser UProp                  -- Parser de la derecha.
       -> Parser UProp
binary f p1 c p2 = (\x1 _ x2 -> f x1 x2) <$> p1 <*> pToken c <*> p2

{-
Me gustaría poder ignorar el whitespace de manera "automática".
No quiero tener que intercalar pWhitespace entre cada parser.
Lo dejo para explorar más adelante.
-}
pWhitespace :: Parser String
pWhitespace = pList (pSym ' ')

{-
prop ::= term "\/" prop | term
term ::= factor "/\" term | factor
factor ::= '∼' prop | '(' prop ')' | '(' prop '=' prop ')' | '(' prop '<' prop ')' | ℕ
-}

data UProp
    = Or UProp UProp
    | And UProp UProp
    | Not UProp
    | Paren UProp   -- Paréntesis
    | Eq UProp UProp
    | Lt UProp UProp
    | N Int
    deriving (Show, Eq)

{-
Cada no terminal de la gramática se corresponde con un parser.
-}

prop :: Parser UProp
prop = pOr <|> term

term :: Parser UProp
term = pAnd <|> factor

factor :: Parser UProp
factor = pNot <|> pParen <|> pEq <|> pLt <|> pN

{-
Definimos los parsers que se corresponden con cada producción de la gramática.
-}

pOr :: Parser UProp
pOr = binary Or term "\\/" prop

pAnd :: Parser UProp
pAnd = binary And factor "/\\" term

pNot :: Parser UProp
pNot = const Not <$> pSym '~' <*> prop

pParen :: Parser UProp
pParen = parenthesis (Paren <$> prop)

pEq :: Parser UProp
pEq = parenthesis (binary Eq prop "=" prop)

pLt :: Parser UProp
pLt = parenthesis (binary Lt prop "<" prop)

pN :: Parser UProp
pN = N <$> number

{-
Entrypoint al parser.
-}

parseExpr :: String -> [(UProp, String)]
parseExpr = runP prop

{-
Algunas expresiones de ejemplo para probar.
-}

e1 = "42"
e2 = "(1<42)"
e3 = "(1=42)"
e4 = "~(1=42)"
e5 = e2 ++ "/\\" ++ e3
e6 = e2 ++ "\\/" ++ e4
e7 = "(1<(42))"

testParser :: String -> UProp -> Bool
testParser input expected = case parseExpr input of
    []            -> False
    (output, r):_ -> output == expected && r == ""

test :: Bool
test = testParser e1 (N 42)
    && testParser e2 (Lt (N 1) (N 42))
    && testParser e3 (Eq (N 1) (N 42))
    && testParser e4 (Not (Eq (N 1) (N 42)))
    && testParser e5 (And (Lt (N 1) (N 42)) (Eq (N 1) (N 42)))
    && testParser e6 (Or (Lt (N 1) (N 42)) (Not (Eq (N 1) (N 42))))
    && testParser e7 (Lt (N 1) (Paren (N 42)))
