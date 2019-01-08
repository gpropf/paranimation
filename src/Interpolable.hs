{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances
, FlexibleInstances#-}

module Interpolable where

import State


type InterpState p v = (p,[(p,v)])


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
                 

--data Interpolable p v = Interpolable p [(p,v)]
