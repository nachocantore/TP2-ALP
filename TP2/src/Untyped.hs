module Untyped where

import           Control.Monad
import           Data.List
import           Data.Maybe

import           Common

------------------------------------------------------------
-- Sección 2
-- Ejercicio 2: Conversión a términos localmente sin nombres
------------------------------------------------------------

conversion :: LamTerm -> Term
conversion t = convaux t []

convaux :: LamTerm -> [String] -> Term
convaux (LVar var)  names = case elemIndex var names of
                              Just n  -> Bound n
                              Nothing -> Free (Global var)
convaux (App t1 t2) names = (convaux t1 names) :@: (convaux t2 names)
convaux (Abs bv t1) names = Lam (convaux t1 (bv : names))

-------------------------------
-- Sección 3
-------------------------------

vapp :: Value -> Value -> Value
vapp (VLam f)     v = f v
vapp (VNeutral n) v = VNeutral (NApp n v)

eval :: NameEnv Value -> Term -> Value
eval e t = eval' t (e, [])

eval' :: Term -> (NameEnv Value, [Value]) -> Value
eval' (Bound ii) (_, lEnv) = lEnv !! ii -- lo cambia por la variable en esta posicion
eval' (Free x) (nvs, lEnv) = case lookup x nvs of
                               Just v  -> v
                               Nothing -> VNeutral (NFree x)
eval' (t1 :@: t2)      env = vapp (eval' t1 env) (eval' t2 env)
eval' (Lam t)  (nvs, lEnv) = VLam (\v->(eval' t (nvs,v:lEnv)))

--eval' _          _         = undefined


--------------------------------
-- Sección 4 - Mostrando Valores
--------------------------------

quote :: Value -> Term
quote = undefined
