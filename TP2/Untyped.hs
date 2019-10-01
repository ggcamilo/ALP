module Untyped where

import Control.Monad
import Data.List

import Common


----------------------------------------------
-- Seccón 2 - Representación de Términos Lambda 
-- Ejercicio 2: Conversión de Términos
----------------------------------------------

getLevel :: NameEnv Int -> String -> Maybe Int
getLevel s v = case ns of
                 [] -> Nothing
                 _  -> Just (foldr1 max ns)
  where ns = [m | (u, m) <- s, u == v]

conversion' :: Int -> NameEnv Int -> LamTerm -> Term
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
shift (Free v)    _ _ = Free v
shift (Bound k)   c d = if k < c then (Bound k) else (Bound $ k+d)
shift (Lam t)     c d = Lam $ shift t c d
shift (t1 :@: t2) c d = (shift t1 c d) :@: (shift t2 c d)


subst' :: Term -> Term -> Int -> Int-> Term
subst' (Free v)    _  _ _ = Free v
subst' (Bound k)   t' i j = if k == i then (shift t' i j) else 
                               if k > i then (Bound $ k-1) else (Bound k)
subst' (Lam t)     t' i j = Lam $ subst' t t' i (j+1)
subst' (t1 :@: t2) t' i j = (subst' t1 t' i j) :@: (subst' t2 t' i j)

subst :: Term -> Term -> Int -> Term
subst t t' i = subst' t t' i 0


eval' :: NameEnv Term -> Int -> Term -> Term
eval' s n (Free v)    = case lookup v s of
                          Nothing -> Free v
                          Just t  -> shift t 0 (n+1)
eval' _ _ (Bound k)   = Bound k
eval' s n ((Lam t1) :@: t2) = eval' s n t
  where t = subst t1 t2 (n+1)
eval' s n (Lam t)     = Lam $ eval' s (n+1) t
eval' s n (t1 :@: t2) = case eval' s n t1 of
                          Lam t -> eval' s n $ (Lam t) :@: t2
                          t     -> t :@: eval' s n t2

eval :: NameEnv Term -> Term -> Term
eval s t = eval' s (-1) t

