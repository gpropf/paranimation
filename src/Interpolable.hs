--{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances
--, FlexibleInstances#-}
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies
    , FlexibleContexts, FlexibleInstances, AllowAmbiguousTypes #-}

module Interpolable where

-- import Control.Monad.State
import Data.List
import Number.Complex

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
  :: (Interpolable a b, Fractional a, Fractional b, Num b) => (a, b) -> (a, b) -> a -> b
linearInterpolate (x1,y1) (x2,y2) x =
  let rs = y2 - y1
      rn = x2 - x1
      m = rs / (x2y rn)
      b = y1 - m * (x2y x1)
      
  in
    m * (x2y x) + b
--    rs
 
instance Interpolable Double Integer where
  x2y = round
  y2x = fromInteger


instance Interpolable Double (Number.Complex.T Double) where
  x2y x = x +: 0
  y2x y = real y

ilc = InterpList [(5::Double, 4.0 +: 0), (15::Double, 0 +: 4)]
il = InterpList [(5::Double, 4.0), (15::Double, 14)]
