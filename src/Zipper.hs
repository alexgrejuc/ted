{-|
Module      : Zipper 
Description : The functions and data types for the creating and manipulating the core data structure
              of the editor.
-}

{-# LANGUAGE ViewPatterns, PatternSynonyms, OverloadedLists, OverloadedStrings #-}

module Zipper where

import Lib

import Data.Sequence (Seq, (<|), (|>), (><), pattern Empty, pattern (:<|), pattern (:|>))
import qualified Data.Sequence as S

import Data.Text (Text, uncons, unsnoc, cons, append)
import qualified Data.Text as T

-- | A 2-d zipper representing the contents of a text file with newlines stripped.
--   A line is represented by the Text to the left of the currently-selected contents, and the Text
--   to its right.
data Zipper =
   Zipper { above     :: Seq Text   -- ^ The lines of text above the current line
          , left      :: Text       -- ^ The text just left of the current selection
          , selection :: Text       -- ^ The current selection
          , right     :: Text       -- ^ The test just right of the current selection
          , below     :: Seq Text   -- ^ The lines of text below the current line
          }
   deriving (Show, Eq)

emptyZipper :: Zipper
emptyZipper = makeZipper [] "" "" "" []

-- | A not-so-smart constructor for a Zipper.
--   If the underlying representation of a Zipper is extended, old
--   examples and test cases might not have to be changed.
makeZipper :: Seq Text -> Text -> Text -> Text -> Seq Text -> Zipper
makeZipper a l s r b = Zipper a l s r b

-- | Makes a Zipper with no lines above or below.
lineZipper :: Text -> Text -> Text -> Zipper
lineZipper l s r = makeZipper [] l s r []

-- | Makes the (left, selection, right) portion of a Zipper at the specified column from a line of
--   text. Should only be used by fromText, assumes no newline character at the end of the input.
--
--   >>> fromLine 1 "123456789" == lineZipper "" "1" "23456789"
--   True
--
fromLine :: Int -> Text -> Zipper
fromLine _ "" = emptyZipper
fromLine x t  = lineZipper "" s r
   where
      (s,r) = T.splitAt x t

-- | Split a Seq Text into (above, selection, below) at the specified row (1-indexed).
splitThree :: Int -> Seq Text -> (Seq Text, Text, Seq Text)
splitThree _ Empty = ([], "", [])
splitThree n l     = case S.splitAt n l of
                        (xs:|>x, r)  -> (xs, x, r)
                        ([x], r)    -> ([], x, r)
                        (Empty, r)  -> ([], "", r) -- n == 0, not an actual use case

-- | Makes a Zipper from Text by splitting on newlines.
--
--   >>> fromText (1,1) "1\n2\n3\n" == makeZipper [] "" "1" "" ["2","3"]
--   True
--
fromText :: (Int, Int) -> Text -> Zipper
fromText _ ""    = emptyZipper
fromText (c,r) t = (fromLine c s) { above = a, below = b }
   where
      ls      = S.fromList $ T.lines t
      (a,s,b) = splitThree c ls

-- | concat, but for a Seq a. I am surprised this does not exist.
concatSeq :: Monoid a => Seq a -> a
concatSeq Empty = mempty
concatSeq xs = foldr mappend mempty xs

-- | Converts a Zipper into Text by interspersing newlines between all lines.
--   This implementation could use an efficiency tune-up, but that can be done later.
--
--   >>> let text = "1\n2\n3\n4\n" in let z = fromText (1,1) text in text == toText z
--   True
toText :: Zipper -> Text
toText (Zipper [] "" "" "" []) = ""
toText z = concatSeq (lines |> (T.pack "\n"))
   where
      between = T.append (left z) (T.append (selection z) (right z))
      lines = S.intersperse (T.pack "\n") (above z >< between <| below z)

-- | Moves the cursor left one column.
--   Does nothing if the cursor is already at the leftmost position.
--
--   >>> goLeft numbers == lineZipper "123" "4" "56789"
--   True
--
--   >>> appN goLeft 20 numbers == lineZipper "" "1" "23456789"
--   True
--
--   >>> toText (goLeft numbers) == toText numbers
--   True
goLeft :: Zipper -> Zipper
goLeft z@(Zipper a l s r b) = case unsnoc l of
                                 Nothing      -> z
                                 Just (l',s') -> Zipper a l' [s'] (append s r) b

-- | Moves the cursor right one column.
--   Does nothing if the cursor is already at the rightmost position.
--
--   >>> goRight numbers == lineZipper "12345" "6" "789"
--   True
--
--   >>> appN goRight 20 numbers == lineZipper "12345678" "9" ""
--   True
--
--   >>> toText (goRight numbers) == toText numbers
--   True
goRight :: Zipper -> Zipper
goRight z@(Zipper a l s r b) = case uncons r of
                                 Nothing       -> z
                                 Just (s', r') -> Zipper a (append l s) [s'] r' b

numbers :: Zipper
numbers = lineZipper "1234" "5" "6789"
