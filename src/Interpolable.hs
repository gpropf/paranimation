--{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances
--, FlexibleInstances#-}
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies
    , FlexibleContexts, AllowAmbiguousTypes #-}

module Interpolable where

import Control.Monad.State


type InterpState p v = [(p,v)]

class (MonadState (InterpState p v) m) => Interpolable p v m where
  getv :: p -> v

instance (Interpolable p v m) (InterpState Double Int) where
  getv p = p
  
{-
--class (State (Interpolable p v) a) => Interpolable p v where
class Eq v => Interpolable p v where
  setp :: p -> InterpState p v -> InterpState p v
  getv ::  InterpState p v -> v
--class (MonadState e Interpolable p v where
  --setp :: p ->  

instance Interpolable Int Double where
  setp pnew ist = let (pold, vs) = ist
                  in
                    (pnew, vs)
                    
  getv ist = let (pold, vs) = ist
                 ((p,v):vs') = vs
             in
               v
           -}      

--data Interpolable p v = Interpolable p [(p,v)]
