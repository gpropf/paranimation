--{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances
--, FlexibleInstances#-}
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies
    , FlexibleContexts, FlexibleInstances, AllowAmbiguousTypes #-}

module Interpolable where

-- import Control.Monad.State
import Data.List

type InterpState x y = [(x,y)]

data InterpList x y = InterpList [(x,y)]

class (Ord x, Eq x, Eq y, Num x) => Interpolable x y where
  x2y :: x -> y
  y2x :: y -> x
  intermediate :: ((x, y) -> (x, y) -> x -> y) -> (InterpList x y) -> x -> y
  intermediate interpFn curvePoints t =
    let (InterpList valueList) = curvePoints
        (lts,gts) = Data.List.partition (\(tn,vn) -> t >= tn) valueList
    in
      case gts of
        [] ->
          let (p:ps) = reverse lts
          in
            interpFn p (head ps) t
        gts' -> interpFn (last lts) (head gts') t

        
linearInterpolate
  :: (Interpolable a b, Fractional a, Num b) => (a, b) -> (a, b) -> a -> b
linearInterpolate (x1,y1) (x2,y2) x =
  let rs = (y2x y2) - (y2x y1)
      rn = x2 - x1
      m = rs / rn
      b = (y2x y1) - m * x1
      
  in
    x2y $ m * x + b
--    rs
 
instance Interpolable Double Integer where
  x2y = round
  y2x = fromInteger
