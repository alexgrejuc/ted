{-|
Module      : Lib
Description : A library of reusable general purpose functions
-}

module Lib where

-- | Successively apply a function n times.
appN :: Integral n => (a -> a) -> n -> a -> a
appN f n z = if n <= 0 then z else appN f (n - 1) (f z)

both :: (a -> b) -> (a, a) -> (b, b)
both f (a,b) = (f a, f b)
