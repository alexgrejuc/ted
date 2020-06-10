{-|
Module      : Lib
Description : A library of reusable general purpose functions
-}

{-# LANGUAGE ViewPatterns, PatternSynonyms #-}
module Lib where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Sequence (Seq, pattern (:<|), pattern (:|>))

-- | Successively apply a function n times.
appN :: Integral n => (a -> a) -> n -> a -> a
appN f n z = if n <= 0 then z else appN f (n - 1) (f z)

-- | Successively apply a function n times or until it reaches a fixpoint
appFix :: (Integral n, Eq a) => (a -> a) -> n -> a -> a
appFix f n z
   | n <= 0    = z
   | otherwise = let z' = f z in if z == z' then z else appFix f (n - 1) z'

-- | Apply a function to both elements of a tuple
both :: (a -> b) -> (a, a) -> (b, b)
both f (a,b) = (f a, f b)

-- | Concatenate the elements of a container with an interspersed element
concatWith :: (Monoid a, Foldable t) => a -> t a -> a
concatWith a xs = foldr (\x acc -> (mappend x a) `mappend` acc) mempty xs

-- | Concatenate the elements of a container. For use with Seq since I couldn't use the prelude
--   version on it
concatS :: (Monoid a, Foldable t) => t a -> a
concatS xs = foldr (\x acc -> x `mappend` acc) mempty xs

-- | Split a Seq a into (init, last)
seqUnsnoc :: Seq a -> (Seq a, a)
seqUnsnoc (xs:|>x) = (xs, x)

-- | Split a Seq a into (head, tail)
seqUncons :: Seq a -> (a, Seq a)
seqUncons (x:<|xs) = (x, xs)

pairDiff :: (Num a, Num b) => (a, b) -> (a, b) -> (a, b)
pairDiff l r = (fst r - fst l, snd r - snd l)

-- | Append operator for Text
(+++) :: Text -> Text -> Text
a +++ b = T.append a b

-- | Rounds up to a positive multiple of some base 
roundUp 0 b = b
roundUp a b = let remainder = a `mod` b in if remainder == 0 then a else a + b - remainder
