module PPLis where

import           AST
import           Text.PrettyPrint
import           Prelude                 hiding ( (<>) )

tabW :: Int
tabW = 4

pVar :: String -> Doc
pVar = text


pSt :: State -> Doc
pSt L  = text "l"
pSt S  = text "s"
pSt G  = text "g"
pSt Aq = text "aq"


pReac :: Reactant -> Doc
pReac (SR (R c i s)) = brackets (int i <> pComp c) <> pSt s
pReac (DR (R c1 i1 s1) (R c2 i2 s2)) = 
      (brackets (int i1 <> pComp c1) <> pSt s1) 
  <+> (brackets (int i2 <> pComp c2) <> pSt s2)


pComp :: Compound -> Doc
pComp (CVar v)   = pVar v
pComp (Simp i v) = int i <> text "*" <> parens (pVar v)
pComp (Comp a b) = pComp a <+> text "+" <+> pComp b


pSamp :: Sample -> Doc
pSamp (SVar     v)       = pVar v
pSamp (DefSamp  v d)     = pVar v <+> braces (double d)
pSamp (ReacSamp m a n b) = 
      text "react" 
  <+> parens (int m <> pSamp a <+> int n <> pSamp b)


pNExp :: ENum -> Doc
pNExp (ConstN i)  = int i
pNExp (ConstD d)  = double d
pNExp (NVar   v)  = pVar v
pNExp (UMinus n)  = text "-" <+> parens (pNExp n)
pNExp (Plus  a b) = pNExp a <+> text "+" <+> pNExp b
pNExp (Times a b) = pNExp a <+> text "*" <+> pNExp b
pNExp (Minus a b) = pNExp a <+> text "-" <+> pNExp b
pNExp (Div   a b) = pNExp a <+> text "/" <+> pNExp b
pNExp (Weight v)  = pVar v <> text "_w"
pNExp (Temp   v)  = pVar v <> text "_t"
pNExp (AtomN  v)  = pVar v <> text "_n"
pNExp (Valen  v)  = pVar v <> text "_v"


pBExp :: EBool -> Doc
pBExp BTrue     = text "true"
pBExp BFalse    = text "false"
pBExp (BVar v)  = pVar v
pBExp (Not  b)  = text "!" <> parens (pBExp b)
pBExp (And a b) = pBExp a <+> text "&"  <+> pBExp b
pBExp (Or  a b) = pBExp a <+> text "|"  <+> pBExp b
pBExp (Eq  a b) = pNExp a <+> text "==" <+> pNExp b
pBExp (Lt  a b) = pNExp a <+> text "<"  <+> pNExp b
pBExp (LEq a b) = pNExp a <+> text "<=" <+> pNExp b
pBExp (Gt  a b) = pNExp a <+> text "<"  <+> pNExp b
pBExp (GEq a b) = pNExp a <+> text ">=" <+> pNExp b


pExp :: Exp -> Doc
pExp (BExp b) = pBExp b
pExp (NExp n) = pNExp n
pExp (CExp c) = pComp c
pExp (SExp s) = pSamp s


pComm :: Comm -> Doc
pComm Skip        = text "skip"
pComm (Let v  e ) = pVar v <+> text "=" <+> pExp e
pComm (Seq c1 c2) = pComm c1 <> text "." $$ pComm c2
pComm (IfThen b c) =
      text "if" <+> parens (pBExp b) 
  <+> braces (nest tabW (pComm c))
pComm (IfThenElse b c1 c2) =
      text "if"
  <+> parens (pBExp b)
  <+> braces (nest tabW (pComm c1))
  <+> text "else"
  <+> braces (nest tabW (pComm c2))
pComm (While b c) =
      text "while" <+> parens (pBExp b) <+> text "do" 
  <+> braces (nest tabW (pComm c))
pComm (Ret e)     = text "ret" <+> parens (pExp e)
pComm (ReacComm (Irr r1 r2)) = pReac r1 <+> text "->"  <+> pReac r2
pComm (ReacComm (Rev r1 r2)) = pReac r1 <+> text "<->" <+> pReac r2


renderComm :: Comm -> String
renderComm = render . pComm

renderExp :: Exp -> String
renderExp = render . pExp

