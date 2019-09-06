module Eval1 (eval) where

import AST

-- Estados
type State = [(Variable,Integer)]

-- Estado nulo
initState :: State
initState = []

-- Busca el valor de una variable en un estado
lookfor :: Variable -> State -> Integer
lookfor v ((u,n):xs) = if (v == u) then n else lookfor v xs

-- Cambia el valor de una variable en un estado
update :: Variable -> Integer -> State -> State
update v i st = ((v,i) : xs)
  where xs = [(x,n) | (x,n) <- st, v /= x]

-- Evalua un programa en el estado nulo
eval :: Comm -> State
eval p = evalComm p initState

-- Evalua un comando en un estado dado
evalComm :: Comm -> State -> State
evalComm Skip s = s
evalComm (Let v iexp) s = update v (evalIntExp iexp s) s
evalComm (Seq comm1 comm2) s = evalComm comm2 (evalComm comm1 s)
evalComm (IfThenElse bexp comm1 comm2) s = if (evalBoolExp bexp s)
                                              then evalComm comm1 s
                                              else evalComm comm2 s
evalComm (While bexp comm) s = if (evalBoolExp bexp s)
                                  then evalComm (While bexp comm) (evalComm comm s)
                                  else s

-- Evalua una expresion entera, sin efectos laterales
evalIntExp :: IntExp -> State -> Integer
evalIntExp (Const  n)         s = n
evalIntExp (Var    v)         s = lookfor v s
evalIntExp (UMinus exp)       s = -(evalIntExp exp s)
evalIntExp (Plus   exp1 exp2) s = (evalIntExp exp1 s)   +   (evalIntExp exp2 s)
evalIntExp (Minus  exp1 exp2) s = (evalIntExp exp1 s)   -   (evalIntExp exp2 s)
evalIntExp (Times  exp1 exp2) s = (evalIntExp exp1 s)   *   (evalIntExp exp2 s)
evalIntExp (Div    exp1 exp2) s = (evalIntExp exp1 s) `div` (evalIntExp exp2 s)

-- Evalua una expresion entera, sin efectos laterales
evalBoolExp :: BoolExp -> State -> Bool
evalBoolExp BTrue             s = True
evalBoolExp BFalse            s = False
evalBoolExp (Eq  iexp1 iexp2) s = (evalIntExp iexp1 s)  == (evalIntExp iexp2 s)
evalBoolExp (NEq iexp1 iexp2) s = (evalIntExp iexp1 s)  /= (evalIntExp iexp2 s)
evalBoolExp (Lt  iexp1 iexp2) s = (evalIntExp iexp1 s)  <  (evalIntExp iexp2 s)
evalBoolExp (Gt  iexp1 iexp2) s = (evalIntExp iexp1 s)  >  (evalIntExp iexp2 s)
evalBoolExp (And bexp1 bexp2) s = (evalBoolExp bexp1 s) && (evalBoolExp bexp2 s)
evalBoolExp (Or  bexp1 bexp2) s = (evalBoolExp bexp1 s) || (evalBoolExp bexp2 s)
evalBoolExp (Not bexp)        s = not (evalBoolExp bexp s)


