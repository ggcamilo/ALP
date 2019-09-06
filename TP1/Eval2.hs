module Eval2 (eval) where

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
lookfor v []         = Left UndefVar
lookfor v ((u,n):xs) = if (v == u) then Right n else lookfor v xs

-- Cambia el valor de una variable en un estado
update :: Variable -> Integer -> State -> State
update v i st = ((v,i) : xs)
  where xs = [(x,n) | (x,n) <- st, v /= x]

-- Evalua un programa en el estado nulo
eval :: Comm -> Either Error State
eval p = evalComm p initState

-- Evalua un comando en un estado dado
evalComm :: Comm -> State -> Either Error State
evalComm Skip s = Right s
evalComm (Let v iexp) s = case (evalIntExp iexp s) of
                            Left err -> Left err
                            Right i  -> Right (update v i s)
evalComm (Seq comm1 comm2) s = case (evalComm comm1 s) of
                                 Left err -> Left err
                                 Right s' -> evalComm comm2 s'
evalComm (IfThenElse bexp comm1 comm2) s = case (evalBoolExp bexp s) of
                                              Left err    -> Left err
                                              Right True  -> evalComm comm1 s
                                              Right False -> evalComm comm2 s
evalComm (While bexp comm) s = case (evalBoolExp bexp s) of
                                 Left err    -> Left err
                                 Right True  -> case (evalComm comm s) of 
                                                  Left err -> Left err
                                                  Right s' -> evalComm (While bexp comm) s'
                                 Right False -> Right s

-- Evalua una expresion entera, sin efectos laterales
evalIntExp :: IntExp -> State -> Either Error Integer
evalIntExp (Const  n)         s = Right n
evalIntExp (Var    v)         s = lookfor v s
evalIntExp (UMinus exp)       s = either1 (0-) (evalIntExp exp s)
evalIntExp (Plus   exp1 exp2) s = either2 (+) (evalIntExp exp1 s) (evalIntExp exp2 s)
evalIntExp (Minus  exp1 exp2) s = either2 (-) (evalIntExp exp1 s) (evalIntExp exp2 s)
evalIntExp (Times  exp1 exp2) s = either2 (*) (evalIntExp exp1 s) (evalIntExp exp2 s)
evalIntExp (Div    exp1 exp2) s = case (evalIntExp exp2 s) of
                                    Left err -> Left err
                                    Right 0  -> Left DivByZero
                                    Right n  -> either2 div (evalIntExp exp1 s) (Right n)

-- Evalua una expresion entera, sin efectos laterales
evalBoolExp :: BoolExp -> State -> Either Error Bool
evalBoolExp BTrue             s = Right True
evalBoolExp BFalse            s = Right False
evalBoolExp (Eq  iexp1 iexp2) s = either2 (==) (evalIntExp iexp1 s) (evalIntExp iexp2 s)
evalBoolExp (NEq iexp1 iexp2) s = either2 (/=) (evalIntExp iexp1 s) (evalIntExp iexp2 s)
evalBoolExp (Lt  iexp1 iexp2) s = either2 (<)  (evalIntExp iexp1 s) (evalIntExp iexp2 s)
evalBoolExp (Gt  iexp1 iexp2) s = either2 (>)  (evalIntExp iexp1 s) (evalIntExp iexp2 s)
evalBoolExp (And bexp1 bexp2) s = either2 (||) (evalBoolExp bexp1 s) (evalBoolExp bexp2 s)
evalBoolExp (Or  bexp1 bexp2) s = either2 (||) (evalBoolExp bexp1 s) (evalBoolExp bexp2 s)
evalBoolExp (Not bexp)        s = either1 not  (evalBoolExp bexp s)

either1 :: (c -> b) -> Either a c -> Either a b
either1 f (Left err) = Left err
either1 f (Right x)  = Right (f x)

either2 :: (c -> c -> b) -> Either a c -> Either a c -> Either a b
either2 _ (Left err) _          = Left err
either2 _ _          (Left err) = Left err
either2 f (Right x)  (Right y)  = Right (f x y)
