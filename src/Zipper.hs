{-# LANGUAGE ViewPatterns, PatternSynonyms, OverloadedLists, OverloadedStrings #-}

module Zipper where

import Data.Sequence (Seq, (<|), (|>), (><))
import qualified Data.Sequence as S
import Data.Text (Text, unsnoc, uncons, cons, append)
import qualified Data.Text as T

import Data.Maybe

-- | For cleaner pattern matching on Data.Sequence
--   thanks to https://stackoverflow.com/a/31106916/13225914
pattern Empty   <- (S.viewl -> S.EmptyL)  where Empty = S.empty
pattern x :< xs <- (S.viewl -> x S.:< xs) where (:<)  = (S.<|)
pattern xs :> x <- (S.viewr -> xs S.:> x) where (:>)  = (S.|>)

-- | A 2-d zipper representing the contents of a text file with newlines stripped.
--   A line is represented by the Text to the left of the currently-selected contents, and the Text
--   to its right.
data Zipper =
   Zipper { above     :: [Text]
          , left      :: Text
          , selection :: Text
          , right     :: Text
          , below     :: [Text]
          }
   deriving (Show, Eq)

-- | Moves the cursor left one column.
--   Does nothing if the cursor is already at the leftmost position.
--
--   >>> goLeft numbers == makeLine "123" "4" "56789"
--   True
--
--   >>> appN goLeft 20 test == makeLine "" "1" "23456789"
--   True
goLeft :: Zipper -> Zipper
goLeft z@(Zipper _ "" _ _ _) = z
goLeft (Zipper a l s r b)    = Zipper a l' [s'] r' b
   where
      (l', s') = fromJust (unsnoc l)
      r'       = append s r

-- | Moves the cursor right one column.
--   Does nothing if the cursor is already at the rightmost position.
--
--   >>> goRight numbers == makeLine "12345" "6" "789"
--   True
--
--   >>> appN goRight 20 test == makeLine "12345678" "9" ""
--   True
goRight :: Zipper -> Zipper
goRight z@(Zipper _ _ _ "" _) = z
goRight (Zipper a l s r b)    = Zipper a l' [s'] r' b
   where
      (s', r') = fromJust (uncons r)
      l'       = append l s

appN :: (a -> a) -> Int -> a -> a
appN f 0 z = z
appN f n z = appN f (n - 1) (f z)

-- | A not-so-smart constructor for a Zipper. If the underlying representation is extended, old
--   examples and test cases might not have to be changed.
makeZipper :: [Text] -> Text -> Text -> Text -> [Text] -> Zipper
makeZipper a l s r b = Zipper a l s r b

-- | Makes a Zipper with no lines above or below.
makeLine :: Text -> Text -> Text -> Zipper
makeLine l s r = makeZipper [] l s r []

numbers :: Zipper
numbers = makeLine "1234" "5" "6789"
