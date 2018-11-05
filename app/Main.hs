module Main where

import Lib
import Params

main :: IO [()]
main = sequence $ writeImageList params "testpic" [0,0.1..3.2]
