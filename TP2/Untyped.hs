module Untyped where

import Control.Monad
import Data.List

import Common


----------------------------------------------
-- Seccón 2 - Representación de Términos Lambda 
-- Ejercicio 2: Conversión de Términos
----------------------------------------------

getLevel :: [(String, Int)] -> String -> Maybe Int
getLevel s v = case ns of
                 [] -> Nothing
                 _  -> Just (foldr1 max ns)
  where ns = [m | (u, m) <- s, u == v]

conversion' :: Int -> [(String, Int)] -> LamTerm -> Term
conversion' n s (LVar v)    = case (getLevel s v) of
                                Just m  -> Bound m
                                Nothing -> Free v
conversion' n s (App t1 t2) = (conversion' n s t1) :@: (conversion' n s t2)
conversion' n s (Abs v t)   = Lam (conversion' n' s' t)
                              where n' = n+1
                                    s' = ((v,n'):s)

conversion  :: LamTerm -> Term
conversion = conversion' (-1) []

  
-------------------------------
-- Sección 3 - Evaluación
-------------------------------

shift :: Term -> Int -> Int -> Term
shift (Free v)    c d = Free v
shift (Bound k)   c d = if k < c then (Bound k)
                                 else (Bound (k+d))
shift (t1 :@: t2) c d = (shift t1 c d) :@: (shift t2 c d)
shift (Lam t)     c d = Lam $ shift t c d
  
  
subst' :: Term -> Term -> Int -> Int -> Term
subst' (Free v)    _  _ _ = Free v
subst' (Bound k)   t' i j = if k < i then (Bound k)
                              else if k > i then (Bound (k-1))
                                     else (shift t' i j)
subst' (t1 :@: t2) t' i j = (subst' t1 t' i j) :@: (subst' t2 t' i j)
subst' (Lam t)     t' i j = Lam $ subst' t t' i (j+1)

subst :: Term -> Term -> Int -> Term
subst t t' i = subst' t t' i 0


eval' :: NameEnv Term -> Int -> Term -> Term
eval' = undefined

eval :: NameEnv Term -> Term -> Term
eval env t = eval' env 0 t
    
    
    
    
    
