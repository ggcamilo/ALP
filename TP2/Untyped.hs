module Untyped where

import Control.Monad
import Data.List

import Common


----------------------------------------------
-- Seccón 2 - Representación de Términos Lambda 
-- Ejercicio 2: Conversión de Términos
----------------------------------------------

searchInScope :: Int -> [(String, Int)] -> String -> Maybe Int
searchInScope n s v = case ns of
                        [] -> Nothing
                        _  -> Just (foldr1 min ns)
  where ns = [m | (u, m) <- s, u == v]

conversion' :: Int -> [(String, Int)] -> LamTerm -> Term
conversion' n s (LVar v)    = case (searchInScope n s v) of
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
shift = undefined
  
  
subst :: Term -> Term -> Int -> Term
subst = undefined


eval :: NameEnv Term -> Term -> Term
eval = undefined
    
    
    
    
    
