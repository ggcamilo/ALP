module Parser where

import Text.ParserCombinators.Parsec
import Text.Parsec.Token
import Text.Parsec.Language (emptyDef)
import AST

-------------------------------------------------
--- Funcion para facilitar el testing del parser.

totParser :: Parser a -> Parser a
totParser p = do whiteSpace lis
                 t <- p
                 eof
                 return t

-- Analizador de Tokens
lis :: TokenParser u
lis = makeTokenParser (emptyDef { commentStart  = "/*"
                                , commentEnd    = "*/"
                                , commentLine   = "//"
                                , opLetter      = char '='
                                , reservedNames = ["true","false","if","then",
                                                     "else", "while"]
                                })

myParens :: Parser a -> Parser a
myParens p = do{ reservedOp lis "("
               ; res <- p
               ; reservedOp lis ")"
               ; return res }

----------------------------------
--- Parser de expressiones enteras
----------------------------------

intexp :: Parser IntExp
intexp = plusOrMinus

plusOrMinus = chainl1 timesOrDiv plusOrMinusOp

timesOrDiv = chainl1 factor timesOrDivOp

factor = int <|> var <|> try (myParens intexp) <|> negated

int = do{ n <- integer lis ; return (Const n) }

var = do{ v <- identifier lis ; return (Var v) }

negated = do{ reservedOp lis "-"
            ; i <- intexp
            ; return (UMinus i) }

plusOrMinusOp =  do{ reservedOp lis "+" ; return Plus }
             <|> do{ reservedOp lis "-" ; return Minus }

timesOrDivOp =  do{ reservedOp lis "*" ; return Times }
            <|> do{ reservedOp lis "/" ; return Div }

------------------------------------
--- Parser de expressiones booleanas
------------------------------------

boolexp :: Parser BoolExp
boolexp = disjunction

disjunction = chainl1 conjunction orOp

conjunction = chainl1 boolTerm andOp

boolTerm = val <|> intexpComp <|> try (myParens boolexp) <|> notOp

intexpComp = do{ exp1 <- intexp
               ; compOp <- compareOp
               ; exp2 <- intexp
               ; return (compOp exp1 exp2) }

val =  do{ reserved lis "true"  ; return BTrue }
   <|> do{ reserved lis "false" ; return BFalse }

notOp = do{ reservedOp lis "!"
          ; b <- boolexp
          ; return (Not b) }

orOp = do{ reservedOp lis "||" ; return Or }

andOp = do{ reservedOp lis "&&" ; return And }

compareOp =  do{ reservedOp lis "==" ; return Eq }
         <|> do{ reservedOp lis "!=" ; return NEq }
         <|> do{ reservedOp lis "<"  ; return Lt }
         <|> do{ reservedOp lis ">"  ; return Gt }

-----------------------------------
--- Parser de comandos
-----------------------------------

comm :: Parser Comm
comm = chainl1 commTerm seqOp

commTerm = skip <|> assign <|> ifComm <|> whileComm

skip = do{ reserved lis "skip" ; return Skip }

assign = do{ v <- identifier lis
           ; l <- letOp
           ; i <- intexp
           ; return (l v i) }

ifComm = try ifthenelse <|> ifthen

ifthenelse = do{ reserved lis "if"
               ; cond <- boolexp
               ; reservedOp lis "{"
               ; then' <- comm
               ; reservedOp lis "}"
               ; reserved lis "else"
               ; reservedOp lis "{"
               ; else' <- comm
               ; reservedOp lis "}"
               ; return (IfThenElse cond then' else') }

ifthen = do{ reserved lis "if"
           ; cond <- boolexp
           ; reservedOp lis "{"
           ; then' <- comm
           ; reservedOp lis "}"
           ; return (IfThenElse cond then' Skip) }

whileComm = do{ reserved lis "while"
              ; cond <- boolexp
              ; reservedOp lis "{"
              ; body <- comm
              ; reservedOp lis "}"
              ; return (While cond body) }

seqOp = do{ reservedOp lis ";" ; return Seq }

letOp = do{ reservedOp lis "=" ; return Let }

------------------------------------
--- Función de parseo
------------------------------------
parseComm :: SourceName -> String -> Either ParseError Comm
parseComm = parse (totParser comm)
