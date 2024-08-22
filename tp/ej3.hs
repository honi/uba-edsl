import Parsing
import Control.Monad

-- Análogo a pSym, pToken consume una cantidad arbitraria de carácteres.
pToken :: String -> Parser String
pToken t = mapM pSat (map (==) t)

pOpen :: Parser Char
pOpen = pSym '('

pClose :: Parser Char
pClose = pSym ')'

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
Dejo comentadas las versiones one liner sin el do notation. Probé las 2 formas
pero todavía no sé cuál es más elegante.
-}

-- pOr = (\t _ p -> Or t p) <$> term <*> pToken "\\/" <*> prop
pOr :: Parser UProp
pOr = do
    t <- term
    pToken "\\/"
    p <- prop
    return (Or t p)

-- pAnd = ((\f _ t -> And f t) <$> factor <*> pToken "/\\" <*> term)
pAnd :: Parser UProp
pAnd = do
    f <- factor
    pToken "/\\"
    t <- term
    return (And f t)

-- pNot = ((\_ p -> Not p) <$> pSym '~' <*> prop)
pNot :: Parser UProp
pNot = do
    pSym '~'
    p <- prop
    return (Not p)

-- pParen = ((\_ p _ -> Paren p) <$> pOpen <*> prop <*> pClose)
pParen :: Parser UProp
pParen = do
    pOpen
    p <- prop
    pClose
    return (Paren p)

-- pEq = ((\_ p _ q _ -> Eq p q) <$> pOpen <*> prop <*> pSym '=' <*> prop <*> pClose)
pEq :: Parser UProp
pEq = do
    pOpen
    p <- prop
    pSym '='
    q <- prop
    pClose
    return (Eq p q)

-- pLt = ((\_ p _ q _ -> Lt p q) <$> pOpen <*> prop <*> pSym '<' <*> prop <*> pClose)
pLt :: Parser UProp
pLt = do
    pOpen
    p <- prop
    pSym '<'
    q <- prop
    pClose
    return (Lt p q)

-- pN = ((\n -> N n) <$> number)
pN :: Parser UProp
pN = do
    n <- number
    return (N n)

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
