{-|
Module      : Zipper
Description : Creation and manipulation of a Text Zipper.

The functions and data types for the creating and manipulating the core data structure
of the editor.
-}

{-# LANGUAGE ViewPatterns, PatternSynonyms, OverloadedLists, OverloadedStrings #-}
module Zipper where

import Lib

import Data.Sequence (Seq, (<|), (|>), (><), pattern Empty, pattern (:<|), pattern (:|>))
import qualified Data.Sequence as S

import Data.Text (Text, snoc, uncons, unsnoc, cons, append)
import qualified Data.Text as T

--import Data.Tuple.Extra

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
--   >>> fromLine 0 "123456789" == lineZipper "" "1" "23456789"
--   True
--
fromLine :: Int -> Text -> Zipper
fromLine _ "" = emptyZipper
fromLine x t  = lineZipper "" s r
   where
      (s,r) = T.splitAt (x + 1) t

-- | Split a Seq Text into (above, selection, below) at the specified row (0-indexed).
splitThree :: Int -> Seq Text -> (Seq Text, Text, Seq Text)
splitThree _ Empty = ([], "", [])
splitThree n l     = case S.splitAt (n + 1) l of
                        (xs:|>x, r)  -> (xs, x, r)
                        ([x], r)    -> ([], x, r)
                        (Empty, r)  -> ([], "", r) -- n == 0, not an actual use case

-- | Makes a Zipper from Text by splitting on newlines.
--
--   >>> fromText (0,0) "1\n2\n3\n" == makeZipper [] "" "1" "" ["2","3"]
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

seqUnsnoc :: Seq a -> (Seq a, a)
seqUnsnoc (xs:|>x) = (xs, x)

seqUncons :: Seq a -> (a, Seq a)
seqUncons (x:<|xs) = (x, xs)

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

-- | Moves the cursor up one row.
--   Does nothing if the cursor is already at the topmost position.
--
--   >>> stepUp (makeZipper ["0", "1"] "1" "2" "3" ["b"]) == makeZipper ["0"] "" "1" "" ["123", "b"]
--   True
--
--   >>> stepUp (makeZipper ["hi"] "1" "2" "3" ["b"]) == makeZipper [] "" "h" "i" ["123", "b"]
--   True
--
--   >>> stepUp (makeZipper ["hello"] "1" "2" "3" []) == makeZipper [] "" "h" "ello" ["123"]
--   True
stepUp :: Zipper -> Zipper
stepUp z@(Zipper a l s r b) = case a of
                                 Empty -> z
                                 _     -> Zipper a' "" s' r' b'
   where
      (a', newLine) = seqUnsnoc a
      (s', r')      = T.splitAt 1 newLine
      oldLine       = T.append l (T.append s r)
      b'            = oldLine <| b

-- | Moves up n rows
goUp :: Integer -> Zipper -> Zipper
goUp = appN stepUp

-- | Moves the cursor down one row.
--   Does nothing if the cursor is already at the bottommost position.
--
--   >>> stepDown (makeZipper ["0"] "" "1" "2" ["b", "c"]) == makeZipper ["0", "12"] "" "b" "" ["c"]
--   True
--
--   >>> stepDown (makeZipper [] "" "!" "" ["single"]) == makeZipper ["!"] "" "s" "ingle" []
--   True
stepDown :: Zipper -> Zipper
stepDown z@(Zipper a l s r b) = case b of
                                 Empty -> z
                                 _     -> Zipper a' "" s' r' b'
   where
      (newLine, b') = seqUncons b
      (s', r')      = T.splitAt 1 newLine
      oldLine       = T.append l (T.append s r)
      a'            = a |> oldLine

-- | Moves down n rows
goDown :: Integer -> Zipper -> Zipper
goDown = appN stepDown

atBottom :: Zipper -> Bool
atBottom (Zipper _ _ _ _ []) = True
atBottom _                   = False

atTop :: Zipper -> Bool
atTop (Zipper [] _ _ _ _) = True
atTop _                   = False

-- | Moves the cursor left one column.
--   Does nothing if the cursor is already at the leftmost position.
--
--   >>> stepLeft numbers == lineZipper "123" "4" "56789"
--   True
--
--   >>> goLeft 20 numbers == lineZipper "" "1" "23456789"
--   True
--
--   >>> toText (stepLeft numbers) == toText numbers
--   True
stepLeft :: Zipper -> Zipper
stepLeft z@(Zipper a l s r b) = case unsnoc l of
                                 Nothing      -> z
                                 Just (l',s') -> Zipper a l' [s'] (append s r) b

-- | Performs a move on a zipper (row, column).
--   Vertically relative, horizontally relative to the x position after the vertical move.
--
--   >>> let z = lineZipper "" "1" "234" in go (1,3) z == z && go (-1,3) z == z
--   True
--
go :: (Integer, Integer) -> Zipper -> Zipper
go (y, x) z
   | y > 0     = if atBottom z then z else go (0, x) $ goDown y z
   | y < 0     = if atTop z then z else go (0, x) $ goUp (abs y) z
   | x > 0     = goRight x z
   | x < 0     = goLeft (abs x) z
   | otherwise = z

-- | Moves left n columns
goLeft :: Integer -> Zipper -> Zipper
goLeft = appN stepLeft

-- | Moves the cursor right one column.
--   Does nothing if the cursor is already at the rightmost position.
--
--   >>> stepRight numbers == lineZipper "12345" "6" "789"
--   True
--
--   >>> goRight 20 numbers == lineZipper "123456789" "" ""
--   True
--
--   >>> toText (stepRight numbers) == toText numbers
--   True
stepRight :: Zipper -> Zipper
stepRight z@(Zipper a l s r b) = case uncons r of
                                 Nothing       -> z { left = append l s, selection = "" }
                                 Just (s', r') -> Zipper a (append l s) [s'] r' b

-- | Moves right n columns
goRight :: Integer -> Zipper -> Zipper
goRight = appN stepRight

-- | Returns a 0-indexed (row, column) cursor position.
--
--   >>> cursorPos emptyZipper == (0,0)
--   True
--
--   >>> cursorPos $ makeZipper ["0", "1", "2", "3"] "01234" "5" "6789" ["below"]
--   (4,5)
--
cursorPos :: Zipper -> (Integer, Integer)
cursorPos (Zipper a l _ _ _) = both fromIntegral (S.length a, T.length l)

-- | Creates a new line from the current selection
--
--   >>> split (lineZipper "moves up" "d" "oesn't") == makeZipper ["moves up"] "" "d" "oesn't" []
--   True
--
--   >>> split (lineZipper "" "stays" "put") == makeZipper [""] "" "stays" "put" []
--   True
split :: Zipper -> Zipper
split (Zipper a l s r b) = Zipper (a |> l) "" s r b

-- | Removes a character from `left`
--
--   >>> backspace (lineZipper "abc" "def" "ghi") == lineZipper "ab" "def" "ghi"
--   True
--
--   >>> backspace (Zipper ["moves"] "" "u" "p" []) == Zipper [] "moves" "u" "p" []
--   True
backspace :: Zipper -> Zipper
backspace z@(Zipper a "" s r b) = case a of
                                    xs:|>x -> z { above = xs, left = x }
                                    _      -> z
backspace z = z { left = T.dropEnd 1 (left z) }

-- | Deletes the current selection
--
--   >>> delete emptyZipper == emptyZipper
--   True
--
--   delete (lineZipper "" "A" "") == emptyZipper
--   True
--
--   >>> delete (lineZipper "abc" "d" "efg") == lineZipper "abc" "e" "fg"
--   True
--
--   >>> delete (lineZipper "abc" "de" "fg") == lineZipper "abc" "f" "g"
--   True
--
--   >>> delete (Zipper [] "abc" "d" "" ["e"]) == Zipper [] "abc" "" "" ["e"]
--   True
--
--   >>> delete (Zipper [] "text" "" "" ["moves up"]) == Zipper [] "text" "m" "oves up" []
--   True
--
delete :: Zipper -> Zipper
delete z@(Zipper _ _ s r b)
   | not (T.null s)  = let (s', r') = T.splitAt 1 r in z { selection = s', right = r' }
   | otherwise       = case b of
                          []     -> z
                          x:<|xs -> z'
                             where
                                (s', r') = T.splitAt 1 x
                                z' = z { selection = s', right = r', below = xs }

appendChar :: Char -> Zipper -> Zipper
appendChar c z = z { left = snoc (left z) c }

appendText :: Text -> Zipper -> Zipper
appendText t z = z { left = T.append (left z) t }

numbers :: Zipper
numbers = lineZipper "1234" "5" "6789"
