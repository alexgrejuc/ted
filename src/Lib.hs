{-|
Module      : Lib
Description : A library of reusable general purpose functions
-}

module Lib where

-- | Successively apply a function n times.
appN :: (a -> a) -> Int -> a -> a
appN f n z = if n <= 0 then z else appN f (n - 1) (f z)
