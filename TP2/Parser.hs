module Parser where

  import Text.Parsec.Prim
  import Text.ParserCombinators.Parsec
  import Text.Parsec.Token
  import Text.Parsec.Language
  import Control.Applicative hiding ((<|>))

  import Common
  import Untyped

-----------------------------------------------
-- Seccón 2 - Representación de Lambda Términos 
-- Ejercicio 1
-----------------------------------------------
  
  appN :: Integer -> LamTerm -> LamTerm -> LamTerm
  appN 0 t1 t2 = t2
  appN n t1 t2 = App t1 $ appN (n-1) t1 t2
  
  num :: Integer -> LamTerm
  num n = Abs "s" $ Abs "z" $ appN n (LVar "s") (LVar "z")

-------------------------------------------------
-- Parser de Lambda Cálculo (Gramatica Extendida) 
-------------------------------------------------

  totParser :: Parser a -> Parser a
  totParser p = do 
                    whiteSpace untyped
                    t <- p
                    eof
                    return t

  -- Analizador de Tokens
  untyped :: TokenParser u
  untyped = makeTokenParser (haskellStyle { identStart = letter <|> char '_'
                                          , opStart    = oneOf "=.\\"
                                          , opLetter = parserZero
                                          , reservedOpNames = ["=",".","\\"]
                                          , reservedNames = ["def"]
                                          })
  
-- Parser para comandos
  parseStmt :: Parser a -> Parser (Stmt a)
  parseStmt p = do
            reserved untyped "def"
            x <- identifier untyped
            reservedOp untyped "="
            t <- p
            return (Def x t)
      <|> fmap Eval p

   
  parseTermStmt :: Parser (Stmt Term)
  parseTermStmt = fmap (fmap conversion) (parseStmt parseLamTerm)

-- Parser para LamTerms 
  parseLamTerm :: Parser LamTerm
  parseLamTerm = do bs <- many1 parseVarAbs
                    return (foldl1 App bs) --foldl1 garantiza la asociación izquierda de la aplicación

  parseVarAbs :: Parser LamTerm
  parseVarAbs = do ide <- identifier untyped
                   return (LVar ide)
            <|> do x <- lexeme untyped (decimal untyped) --lexeme toma un parser y devuelve otro que consume todos los espacios (tabuladores y newlines), a la derecha, del token parseado
                   return (num (fromInteger x))
            <|> do reservedOp untyped "\\"
                   vars <- many1 (identifier untyped)
                   reservedOp untyped "."
                   lt <- parseLamTerm
                   return (foldr Abs lt vars)
            <|> parens untyped parseLamTerm

-- para testear el parser interactivamente.
  testParser :: Parser LamTerm
  testParser = totParser parseLamTerm
