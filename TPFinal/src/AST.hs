module AST where

-- Identificadores de simbolos quimicos
type Symbol   = String
type Variable = String
type Element = Symbol


data State = L | G | S | Aq
  deriving (Eq, Show)

data Compound
    = CVar Symbol
    | Simp   Int      Element
    | Comp   Compound Compound
  deriving (Eq, Show)

data Sample
    = SVar Symbol
    | DefSamp  Symbol Double
    | ReacSamp Int Sample Int Sample
  deriving (Eq, Show)

data Reactant2 = R Compound Int State
  deriving (Eq, Show)
data Reactant  = SR Reactant2
               | DR Reactant2 Reactant2
  deriving (Eq, Show)

data Reaction
    = Irr Reactant Reactant
    | Rev Reactant Reactant
  deriving (Eq, Show)

-- Expresiones, aritmeticas y booleanas
data ENum
    -- Nat
    = ConstN Int
    | AtomN  Symbol
    | Valen  Symbol
    -- Double
    | ConstD Double
    | Weight Symbol
    | Temp   Symbol
    -- Numeros en general
    | NVar   Variable
    | UMinus ENum
    | Plus   ENum ENum
    | Minus  ENum ENum
    | Times  ENum ENum
    | Div    ENum ENum
  deriving (Eq, Show)

data EBool
    = BTrue
    | BFalse
    | BVar   Variable
    | And    EBool EBool
    | Or     EBool EBool
    | Not    EBool
    | Lt     ENum  ENum
    | LEq    ENum  ENum
    | Gt     ENum  ENum
    | GEq    ENum  ENum
    | Eq     ENum  ENum
  deriving (Eq, Show)

data Exp = CExp Compound | SExp Sample | NExp ENum | BExp EBool
  deriving (Eq, Show)

-- Comandos (sentencias)
-- Observar que solo se permiten variables de un tipo (entero)
data Comm
    = Skip
    | Let Variable Exp
    | Seq Comm Comm
    | IfThenElse EBool Comm Comm
    | While      EBool Comm
    | Ret        Exp
    | ReacComm Reaction
  deriving (Eq, Show)

pattern IfThen :: EBool -> Comm -> Comm
pattern IfThen b c = IfThenElse b c Skip

data Error = DivByZero | UndefVar | UndefSymbol | NotElement | NotCompound | ValencesNotMatching 
  deriving (Eq, Show)

