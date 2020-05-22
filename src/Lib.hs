{-|
Module      : Lib
Description : A library of reusable general purpose functions
-}

{-# LANGUAGE ViewPatterns, PatternSynonyms #-}
module Lib where

import Data.Sequence (Seq, pattern (:<|), pattern (:|>))

-- | Successively apply a function n times.
appN :: Integral n => (a -> a) -> n -> a -> a
appN f n z = if n <= 0 then z else appN f (n - 1) (f z)

-- | Successively apply a function n times or until it reaches a fixpoint
appFix :: (Integral n, Eq a) => (a -> a) -> n -> a -> a
appFix f n z
   | n <= 0    = z
   | otherwise = let z' = f z in if z == z' then z else appFix f (n - 1) z'

both :: (a -> b) -> (a, a) -> (b, b)
both f (a,b) = (f a, f b)

concatWith :: (Monoid a, Foldable t) => a -> t a -> a
concatWith a xs = foldr (\x acc -> (mappend x a) `mappend` acc) mempty xs

seqUnsnoc :: Seq a -> (Seq a, a)
seqUnsnoc (xs:|>x) = (xs, x)

seqUncons :: Seq a -> (a, Seq a)
seqUncons (x:<|xs) = (x, xs)
