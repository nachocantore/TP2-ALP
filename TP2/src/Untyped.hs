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
conversion (LVar var)      = Free (Global var)
conversion (App t u)       = (conversion t) :@: (conversion u)
conversion (Abs bound1 t1) = Lam (auxconversion t1 [bound1]) where
                               auxconversion (LVar var) names      = case elemIndex var names of
                                                                       Just n  -> Bound n
                                                                       Nothing -> Free (Global var)
                               auxconversion (App t u) names       = (auxconversion t names) :@: (auxconversion u names)
                               auxconversion (Abs bound2 t2) names = Lam (auxconversion t2 (bound2 : names))

-------------------------------
-- Sección 3
-------------------------------

vapp :: Value -> Value -> Value
vapp = undefined

eval :: NameEnv Value -> Term -> Value
eval e t = eval' t (e, [])

eval' :: Term -> (NameEnv Value, [Value]) -> Value
eval' (Bound ii) (_, lEnv) = lEnv !! ii
eval' _          _         = undefined


--------------------------------
-- Sección 4 - Mostrando Valores
--------------------------------

quote :: Value -> Term
quote = undefined
