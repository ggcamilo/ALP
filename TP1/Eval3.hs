module Eval3 (eval) where

import AST

data Error = DivByZero | UndefVar
  deriving Show

-- Estados
type State = [(Variable,Integer)]

-- Estado nulo
initState :: State
initState = []

-- Busca el valor de una variable en un estado
lookfor :: Variable -> State -> Either Error Integer
lookfor v [] = Left UndefVar
lookfor v ((u,n):xs) = if (v == u)
                          then Right n
                          else lookfor v xs

-- Cambia el valor de una variable en un estado
update :: Variable -> Integer -> State -> State
update v i st = ((v,i) : xs)
  where xs = [(x,n) | (x,n) <- st, v /= x]

-- Evalua un programa en el estado nulo
eval :: Comm -> Either Error (State, Integer)
eval p = evalComm p (initState,0)

-- Evalua un comando en un estado dado
evalComm :: Comm -> (State, Integer) -> Either Error (State, Integer)
evalComm Skip st = Right st
evalComm (Let v iexp) (s,c) = case (evalIntExp iexp s) of
                                Left err     -> Left err
                                Right (i,c2) -> Right ((update v i s),c+c2)
evalComm (Seq comm1 comm2) (s,c) = case (evalComm comm1 (s,c)) of
                                     Left err      -> Left err
                                     Right (s',c2) -> evalComm comm2 (s',c+c2)
evalComm (IfThenElse bexp comm1 comm2) (s,c) = case (evalBoolExp bexp s) of
                                                 Left err         -> Left err
                                                 Right (True,c2)  -> evalComm comm1 (s,c+c2)
                                                 Right (False,c2) -> evalComm comm2 (s,c+c2)
evalComm (While bexp comm) (s,c) = case (evalBoolExp bexp s) of
                                     Left err         -> Left err
                                     Right (True,c2)  -> case (evalComm comm (s,c)) of 
                                                           Left error    -> Left error
                                                           Right (s',c3) -> evalComm (While bexp comm) (s',c+c2+c3)
                                     Right (False,c2) -> Right (s,c+c2)

-- Evalua una expresion entera, sin efectos laterales
evalIntExp :: IntExp -> State -> Either Error (Integer, Integer)
evalIntExp (Const  n)         s = Right (n, 0)
evalIntExp (Var    v)         s = case (lookfor v s) of
                                    Left err -> Left err
                                    Right n  -> Right (n, 0)
evalIntExp (UMinus exp)       s = either1 (0-) 1 (evalIntExp exp s)
evalIntExp (Plus   exp1 exp2) s = either2 (+)  1 (evalIntExp exp1 s) (evalIntExp exp2 s)
evalIntExp (Minus  exp1 exp2) s = either2 (-)  1 (evalIntExp exp1 s) (evalIntExp exp2 s)
evalIntExp (Times  exp1 exp2) s = either2 (*)  2 (evalIntExp exp1 s) (evalIntExp exp2 s)
evalIntExp (Div    exp1 exp2) s = case (evalIntExp exp2 s) of
                                    Left err    -> Left err
                                    Right (0,_) -> Left DivByZero
                                    Right (n,c) -> either2 div 2 (evalIntExp exp1 s) (Right (n,c))

-- Evalua una expresion entera, sin efectos laterales
evalBoolExp :: BoolExp -> State -> Either Error (Bool, Integer)
evalBoolExp BTrue             s = Right (True,0)
evalBoolExp BFalse            s = Right (False,0)
evalBoolExp (Eq  iexp1 iexp2) s = either2 (==) 1 (evalIntExp iexp1 s) (evalIntExp iexp2 s)
evalBoolExp (NEq iexp1 iexp2) s = either2 (/=) 1 (evalIntExp iexp1 s) (evalIntExp iexp2 s)
evalBoolExp (Lt  iexp1 iexp2) s = either2 (<)  1 (evalIntExp iexp1 s) (evalIntExp iexp2 s)
evalBoolExp (Gt  iexp1 iexp2) s = either2 (>)  1 (evalIntExp iexp1 s) (evalIntExp iexp2 s)
evalBoolExp (And bexp1 bexp2) s = either2 (&&) 1 (evalBoolExp bexp1 s) (evalBoolExp bexp2 s)
evalBoolExp (Or  bexp1 bexp2) s = either2 (||) 1 (evalBoolExp bexp1 s) (evalBoolExp bexp2 s)
evalBoolExp (Not bexp)        s = either1 not  1 (evalBoolExp bexp s)

either1 :: Num d => (b -> c) -> d -> Either a (b, d) -> Either a (c, d)
either1 _ _   (Left err)     = Left err
either1 f cOp (Right (x, n)) = Right ((f x), n+cOp)

either2 :: Num d => (b -> b -> c) -> d -> Either a (b, d) -> Either a (b, d) -> Either a (c, d)
either2 _ _   (Left err)     _             = Left err
either2 _ _   _              (Left err)    = Left err
either2 f cOp (Right (x, n)) (Right (y,m)) = Right ((f x y), n+m+cOp)


