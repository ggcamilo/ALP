{
module Parse where
import AST
import Data.Maybe
import Data.Char
}

%monad { P } { thenP } { returnP }
%name parserComm Comm

%tokentype { Token }
%lexer {lexer} {TEOF}

%token
    '='     { TAssign }
    '.'     { TDot    }
    '_n'    { TAtomN  }
    '_v'    { TValen  }
    '_t'    { TTemp   }
    '_w'    { TWeight }
    ';'     { TSColon }
    '+'     { TPlus   }
    '-'     { TMinus  }
    '*'     { TTimes  }
    '/'     { TDiv    }
    '=='    { TEquals }
    '<'     { TLT     }
    '<='    { TLEq    }
    '>'     { TGT     }
    '>='    { TGEq    }
    '|'     { TOr     }
    '&'     { TAnd    }    
    '!'     { TNot    }
    '('     { TOpenP  }
    ')'     { TCloseP }
    '['     { TOpenB  }
    ']'     { TCloseB }
    '{'     { TOpenC  }
    '}'     { TCloseC }
    '->'    { TArrow  }
    '<->'   { TArrowD }
    NAT     { TNat    $$ }
    DOUBLE  { TDouble $$ }
    STR     { TStr    $$ }
    S       { TSolid  }
    L       { TLiquid }
    G       { TGas    }
    AQ      { TAq     }
    REACT   { TReact  }
    RET     { TRet    }
    IF      { TIf     }
    THEN    { TThen   }
    ELSE    { TElse   }
    WHILE   { TWhile  }
    DO      { TDo     }
    SKIP    { TSkip   }
    TRUE    { TTrue   }
    FALSE   { TFalse  }
    

%right '.'
%right '|' '&'
%left '+' '-'
%left '*' '/'

%%

Comm : SKIP            { Skip }
     | STR  '=' Exp    { Let  $1 $3 }
     | Comm '.' Comm   { Seq  $1 $3 }
     | RET Exp         { Ret  $2 }
     | IF    EBool THEN  '{' Comm '}'                    { IfThen     $2 $5 }
     | IF    EBool THEN  '{' Comm '}' ELSE '{' Comm '}'  { IfThenElse $2 $5 $9 }
     | WHILE EBool DO    '{' Comm '}'                    { While      $2 $5 }
     | Reaction        { ReacComm $1 }

Exp  : EBool     { BExp $1 }
     | ENum      { NExp $1 }
     | Compound  { CExp $1 }
     | Sample    { SExp $1 }
     
EBool : TRUE       { BTrue  }
      | FALSE      { BFalse }
      | STR        { BVar  $1 }
      | '!' EBool  { Not   $2 }
      | EBool '|'  EBool  { Or    $1 $3 }
      | EBool '&'  EBool  { And   $1 $3 }
      | ENum  '<'  ENum   { Lt    $1 $3 }
      | ENum  '<=' ENum   { LEq   $1 $3 }
      | ENum  '>'  ENum   { Gt    $1 $3 }
      | ENum  '>=' ENum   { GEq   $1 $3 }
      | ENum  '==' ENum   { Eq    $1 $3 }

ENum : NAT       { ConstN  $1 }
     | DOUBLE    { ConstD  $1 }
	 | STR '_n'  { AtomN   $1 }
	 | STR '_v'  { Valen   $1 }
	 | STR '_t'  { Temp    $1 }
	 | STR '_w'  { Weight  $1 }
	 | STR       { NVar    $1 }
	 | '-' ENum  { UMinus  $2 }
	 | ENum '+' ENum { Plus  $1 $3 }
	 | ENum '-' ENum { Minus $1 $3 }
	 | ENum '*' ENum { Times $1 $3 }
	 | ENum '/' ENum { Div   $1 $3 }

Reaction  : Reactant '->'  Reactant  { Irr $1 $3 }
          | Reactant '<->' Reactant  { Rev $1 $3 }

Reactant  : Reactant2 '+' Reactant2  { DR $1 $3 }
          | Reactant2  {SR $1}
Reactant2 : Compound NAT State       { R $1 $2 $3 }

Sample : STR  { SVar $1 }
       | Compound '{' DOUBLE '}'  { DefSamp $1 $3 }
       | REACT '(' NAT '*' Sample '+' NAT '*' Sample ')'  { ReacSamp $3 $5 $7 $9 }

Compound : STR  { CVar $1 }
         | NAT      '*' STR          { Simp $1 $3 }
         | Compound '+' Compound     { Comp $1 $3 }

State : S   { $1 }
      | L   { $1 }
      | G   { $1 }
      | AQ  { $1 }

{

data ParseResult a = Ok a | Failed String
                     deriving Show                     
type LineNumber = Int
type P a = String -> LineNumber -> ParseResult a

getLineNo :: P LineNumber
getLineNo = \s l -> Ok l

thenP :: P a -> (a -> P b) -> P b
m `thenP` k = \s l-> case m s l of
                         Ok a     -> k a s l
                         Failed e -> Failed e
                         
returnP :: a -> P a
returnP a = \s l-> Ok a

failP :: String -> P a
failP err = \s l -> Failed err

catchP :: P a -> (String -> P a) -> P a
catchP m k = \s l -> case m s l of
                        Ok a     -> Ok a
                        Failed e -> k e s l

happyError :: P a
happyError = \ s i -> Failed $ "Línea "++(show (i::LineNumber))++": Error de parseo\n"++(s)

data Token = TAssign
           | TDot
           | TAtomN
           | TValen
           | TTemp
           | TWeight
           | TSColon
           | TPlus
           | TMinus
           | TTimes
           | TDiv
           | TEquals
           | TLT
           | TLEq
           | TGT
           | TGEq
           | TOr
           | TAnd
           | TNot
           | TOpenP
           | TCloseP
           | TOpenB
           | TCloseB
           | TOpenC
           | TCloseC
           | TArrow
           | TArrowD
           | TNat    Int
           | TDouble Double
           | TStr    String
           | TSolid
           | TLiquid
           | TGas
           | TAq
           | TReact
           | TRet
           | TIf
           | TThen
           | TElse
           | TWhile
           | TDo
           | TSkip
           | TTrue
           | TFalse
           | TEOF
    deriving Show

----------------------------------
lexer cont s = case s of
                 [] -> cont TEOF []
                 ('\n':s)  ->  \line -> lexer cont s (line + 1)
                 (c:cs) | isSpace c -> lexer cont cs
                        | isAlpha c -> lexVar (c:cs)
                 ('-':('-':cs)) -> lexer cont $ dropWhile ((/=) '\n') cs
                 ('{':('-':cs)) -> consumirBK 0 0 cont cs	
                 ('-':('}':cs)) -> \ line -> Failed $ "Línea "++(show line)++": Comentario no abierto"
                 ('-':('>':cs)) -> cont TArrow  cs
                 ('<':('-':('>':cs))) -> cont TArrowD cs
                 ('.':cs) -> cont TDot    cs
                 ('_':('n':cs)) -> cont TAtomN  cs
                 ('_':('v':cs)) -> cont TValen  cs
                 ('_':('t':cs)) -> cont TTemp   cs
                 ('_':('w':cs)) -> cont TWeight cs
                 (';':cs) -> cont TSColon cs
                 ('+':cs) -> cont TPlus   cs
                 ('-':cs) -> cont TMinus  cs
                 ('*':cs) -> cont TTimes  cs
                 ('/':cs) -> cont TDiv    cs
                 ('=':('=':cs)) -> cont TEquals cs
                 ('=':cs) -> cont TAssign cs
                 ('>':('=':cs)) -> cont TGEq    cs
                 ('>':cs) -> cont TGT     cs
                 ('<':('=':cs)) -> cont TLEq    cs
                 ('<':cs) -> cont TLT     cs
                 ('|':cs) -> cont TOr     cs
                 ('&':cs) -> cont TAnd    cs
                 ('!':cs) -> cont TNot    cs
                 ('(':cs) -> cont TOpenP  cs
                 (')':cs) -> cont TCloseP cs
                 ('{':cs) -> cont TOpenC  cs
                 ('}':cs) -> cont TCloseC cs
                 ('[':cs) -> cont TOpenB  cs
                 (']':cs) -> cont TCloseB cs
                 unknown -> \line -> Failed $ "Línea "++(show line)++": No se puede reconocer "++(show $ take 10 unknown)++ "..."
  where lexVar cs = case span isAlpha cs of
                      ("s",     rest) -> cont TSolid  rest
                      ("l",     rest) -> cont TLiquid rest
                      ("g",     rest) -> cont TGas    rest
                      ("aq",    rest) -> cont TAq     rest
                      ("react", rest) -> cont TReact  rest
                      ("ret",   rest) -> cont TRet    rest
                      ("if",    rest) -> cont TIf     rest
                      ("then",  rest) -> cont TThen   rest
                      ("else",  rest) -> cont TElse   rest
                      ("while", rest) -> cont TWhile  rest
                      ("do",    rest) -> cont TDo     rest
                      ("true",  rest) -> cont TTrue   rest
                      ("false", rest) -> cont TFalse  rest
                      (var,     rest) -> cont (TStr var) rest
        lexDigit cs = case span isDigit cs of
                        (digit, rest) -> cont (TNat digit) rest
        consumirBK anidado cl cont s = case s of
                                         ('-':('-':cs)) -> consumirBK anidado cl cont $ dropWhile ((/=) '\n') cs
                                         ('{':('-':cs)) -> consumirBK (anidado+1) cl cont cs	
                                         ('-':('}':cs)) -> case anidado of
                                                             0 -> \line -> lexer cont cs (line+cl)
                                                             _ -> consumirBK (anidado-1) cl cont cs
                                         ('\n':cs) -> consumirBK anidado (cl+1) cont cs
                                         (_:cs) -> consumirBK anidado cl cont cs     


parseComm s = parserComm s 1
}
