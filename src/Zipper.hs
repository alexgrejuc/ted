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

-- | A 2-d zipper representing the contents of a text file with newlines stripped.
--   A paragraph is represented by the Text to the left of the currently-selected contents, and the
--   Text to its right.
data Zipper =
   Zipper { above     :: Seq Text   -- ^ The paragraphs of text above the current line
          , left      :: Text       -- ^ The text left and above the current selection
          , selection :: Text       -- ^ The current selection
          , right     :: Text       -- ^ The text right and below the current selection
          , below     :: Seq Text   -- ^ The paragraphs of text below the current line
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
paragraph :: Text -> Text -> Text -> Zipper
paragraph l s r = makeZipper [] l s r []

-- | Makes the (left, selection, right) portion of a Zipper at the specified column from a line of
--   text.
--
--   >>> fromLine 0 "123456789" == paragraph "" "1" "23456789"
--   True
--
fromLine :: Integer -> Text -> Zipper
fromLine _ "" = emptyZipper
fromLine x t  = paragraph "" s r
   where
      (s,r) = T.splitAt (fromIntegral x + 1) t

-- | Split a Seq Text into (above, selection, below) at the specified row (0-indexed).
splitThree :: Integer -> Seq Text -> (Seq Text, Text, Seq Text)
splitThree _ Empty = ([], "", [])
splitThree n l     = case S.splitAt (fromIntegral n + 1) l of
                        (xs:|>x, r)  -> (xs, x, r)
                        ([x], r)    -> ([], x, r)
                        (Empty, r)  -> ([], "", r) -- n == 0, not an actual use case

-- | Makes a Zipper from Text by splitting on newlines.
--
--   >>> fromText (0,0) "1\n2\n3\n" == makeZipper [] "" "1" "" ["2","3"]
--   True
--
fromText :: (Integer, Integer) -> Text -> Zipper
fromText _ ""    = emptyZipper
fromText (c,r) t = (fromLine c s) { above = a, below = b }
   where
      ls      = S.fromList $ T.lines t
      (a,s,b) = splitThree c ls

combineLine :: Zipper -> Text
combineLine z = T.append (left z) (T.append (selection z) (right z))

toLines :: Zipper -> Seq Text
toLines z = above z >< combineLine z <| below z

-- | Converts a Zipper into Text by interspersing newlines between all lines.
--   This implementation could use an efficiency tune-up, but that can be done later.
--
--   >>> let text = "1\n2\n3\n4\n" in let z = fromText (1,1) text in text == toText z
--   True
toText :: Zipper -> Text
toText (Zipper [] "" "" "" []) = ""
toText z = concatWith "\n" $ toLines z

displayLines :: Zipper -> Seq Text
displayLines z = fmap (\l -> if l == "" then " " else l) (toLines z)

toRows :: Integer -> Zipper -> Seq Text
toRows l z = displayLines z >>= fmap S.fromList (T.chunksOf (fromIntegral l))

atBottom :: Zipper -> Bool
atBottom (Zipper _ _ _ _ []) = True
atBottom _                   = False

atTop :: Zipper -> Bool
atTop (Zipper [] _ _ _ _) = True
atTop _                   = False

atEnd :: Zipper -> Bool
atEnd (Zipper [] _ "" "" _) = True
atEnd _ = False

-- | Moves the cursor up one row.
--   Does nothing if the cursor is already at the topmost position.
--
--   >>> upPar (makeZipper ["0", "1"] "1" "2" "3" ["b"]) == makeZipper ["0"] "" "1" "" ["123", "b"]
--   True
--
--   >>> upPar (makeZipper ["hi"] "1" "2" "3" ["b"]) == makeZipper [] "" "h" "i" ["123", "b"]
--   True
--
--   >>> upPar (makeZipper ["hello"] "1" "2" "3" []) == makeZipper [] "" "h" "ello" ["123"]
--   True
upPar :: Zipper -> Zipper
upPar z@(Zipper a l s r b) = case a of
                                 Empty -> z
                                 _     -> Zipper a' "" s' r' b'
   where
      (a', newLine) = seqUnsnoc a
      (s', r')      = T.splitAt 1 newLine
      oldLine       = T.append l (T.append s r)
      b'            = oldLine <| b

-- | Moves up n rows
upPars :: Integer -> Zipper -> Zipper
upPars = appFix upPar

-- | Moves the cursor down one row.
--   Does nothing if the cursor is already at the bottommost position.
--
--   >>> downPar (makeZipper ["0"] "" "1" "2" ["b", "c"]) == makeZipper ["0", "12"] "" "b" "" ["c"]
--   True
--
--   >>> downPar (makeZipper [] "" "!" "" ["single"]) == makeZipper ["!"] "" "s" "ingle" []
--   True
downPar :: Zipper -> Zipper
downPar z@(Zipper a l s r b) = case b of
                                 Empty -> z
                                 _     -> Zipper a' "" s' r' b'
   where
      (newLine, b') = seqUncons b
      (s', r')      = T.splitAt 1 newLine
      oldLine       = T.append l (T.append s r)
      a'            = a |> oldLine

-- | Moves down n rows
downPars :: Integer -> Zipper -> Zipper
downPars = appFix downPar

-- | Moves the cursor left one column or to the end of the previous paragraph if at the leftmost
--   position
--
--   >>> stepLeft numbers == paragraph "123" "4" "56789"
--   True
--
--   >>> goLeft 20 numbers == paragraph "" "1" "23456789"
--   True
--
--   >>> toText (stepLeft numbers) == toText numbers
--   True
stepLeft :: Zipper -> Zipper
stepLeft z@(Zipper a l s r b) =
   case unsnoc l of
      Just (l',s') -> Zipper a l' [s'] (append s r) b
      Nothing      -> case a of
                        []     -> z
                        xs:|>x -> Zipper xs x "" "" ((combineLine z) <| b)

-- | Moves left n times
goLeft :: Integer -> Zipper -> Zipper
goLeft = appFix stepLeft

-- | Moves the cursor right one column.
--   Does nothing if the cursor is already at the rightmost position.
--
--   >>> stepRight numbers == paragraph "12345" "6" "789"
--   True
--
--   >>> goRight 20 numbers == paragraph "123456789" "" ""
--   True
--
--   >>> toText (stepRight numbers) == toText numbers
--   True
stepRight :: Zipper -> Zipper
stepRight z@(Zipper a l s r b) = case uncons r of
                  Nothing -> if T.null s then downPar z else Zipper a (T.append l s) "" "" b
                  Just (s', r') -> Zipper a (append l s) [s'] r' b

-- | Moves right n columns
goRight :: Integer -> Zipper -> Zipper
goRight = appFix stepRight

-- | Performs a move on a zipper (row, column).
--   Vertically relative, horizontally relative to the x position after the vertical move.
go :: Integer -> (Integer, Integer) -> Zipper -> Zipper
go l (y, x) z
   | y > 0     = go l (0, x) $ goDown y l z
   | y < 0     = go l (0, x) $ goUp (abs y) l z 
   | x > 0     = goRight x z
   | x < 0     = goLeft (abs x) z
   | otherwise = z

-- | Moves down one visual line (e.g. n characters, the length of a line on the screen)
--
--   >>> stepDown 5 (paragraph "" "l" "ine line") == paragraph "line " "l" "ine"
--   True
--
--   >>> stepDown 20 (Zipper [] "between para" "" "" [""]) == Zipper ["between para"] "" "" "" []
--   True
--
--   >>> stepDown 20 (Zipper [] "between para" "" "" ["!"]) == Zipper ["between para"] "!" "" "" []
--   True
--
--   >>> stepDown 20 (Zipper [] "be" "t" "ween" ["!!!"]) == Zipper ["between"] "!!" "!" "" []
--   True
--
--   >>> stepDown 4 (Zipper [] "lin" "e" "l" ["not here"]) == Zipper [] "linel" "" "" ["not here"]
--   True
stepDown :: Integer -> Zipper -> Zipper
stepDown len z@(Zipper a l s r b) = if atBottom then drop else wrap
   where
      llen       = fromIntegral len
      leftChars  = T.length l
      col        = leftChars `mod` llen
      atBottom   = col + 1 + (T.length s) + (T.length r) < llen
      (l', r')   = T.splitAt (leftChars + llen) (combineLine z)
      wrap       = z { left = l', selection = T.take 1 r', right = T.drop 1 r' } 
      drop       = case b of
                     []       -> z
                     "":<|xs  -> downPar z
                     x:<|xs   -> Zipper (a |> combineLine z) l' s' (T.append r'' rest) xs
                        where
                           (top, rest)  = T.splitAt llen x
                           (l', r')     = T.splitAt col top
                           (s', r'')    = T.splitAt 1 r' 

goDown :: Integer -> Integer -> Zipper -> Zipper 
goDown n len z = appFix (stepDown len) n z 

-- | Moves up one visual line (e.g. n characters, the length of a line on the screen)
--
--   >>> stepUp 2 (Zipper [] "abcde" "f" "" []) == Zipper [] "abc" "d" "ef" []
--   True
--
--   >>> stepUp 2 (Zipper [""] "" "a" "b" []) == Zipper [] "" "" "" ["ab"]
--   True
--
--   >>> stepUp 10 (Zipper ["abcdef"] "" "g" "h" []) == Zipper [] "" "a" "bcdef" ["gh"]
--   True
--
--   >>> stepUp 10 (Zipper ["ab"] "cdef" "g" "h" []) == Zipper [] "ab" "" "" ["cdefgh"]
--   True
--
stepUp :: Integer -> Zipper -> Zipper
stepUp len z@(Zipper a l s r _)
   | charsLeft >= llen = let (l', r') = T.splitAt (charsLeft - llen) l in
                         let r''      = T.append (T.drop 1 r') (T.append s r) in
                             z { left = l', selection = T.take 1 r', right = r'' }
   | otherwise = case a of
                  []      -> z
                  xs:|>"" -> upPar z
                  xs:|>x   -> z { above = xs, left = l', selection = s', right = r', below = b' }
                     where
                        (above, last)  = let len = T.length x in T.splitAt (len - len `mod` llen) x
                        column         = (charsLeft `mod` llen)
                        (lastl, lastr) = T.splitAt column last
                        l'             = T.append above lastl
                        (s', r')       = T.splitAt 1 (lastr)
                        b'             = (combineLine z) <| (below z)
   where
      charsLeft = (T.length l)
      llen = fromIntegral len

goUp :: Integer -> Integer -> Zipper -> Zipper
goUp n len z = appFix (stepUp len) n z

roundUp 0 b = b
roundUp a b = let remainder = a `mod` b in if remainder == 0 then a else a + b - remainder

-- | Returns a 0-indexed (row, column) cursor position based on lines of length n
--
--   >>> cursorPos 10 emptyZipper == (0,0)
--   True
--
--   >>> cursorPos 10 $ makeZipper ["0", "1", "2", "3"] "01234" "5" "6789" ["below"]
--   (4,5)
--
--   >>> cursorPos 2 $ makeZipper ["0", "1", "2", "3"] "01234" "5" "6789" ["below"]
--   (6,1)
--
cursorPos :: Integer -> Zipper -> (Integer, Integer)
cursorPos len (Zipper a l _ _ _) = both fromIntegral (row, col)
   where
      len' = fromIntegral len
      prevChars = foldr (\p sum -> (roundUp (T.length p) len' + sum)) 0 a
      row = prevChars `div` len' + (T.length l `div` len')
      col = T.length l `mod` len'

-- | Creates a new line from the current selection
--
--   >>> split (paragraph "moves up" "d" "oesn't") == makeZipper ["moves up"] "" "d" "oesn't" []
--   True
--
--   >>> split (paragraph "" "stays" "put") == makeZipper [""] "" "stays" "put" []
--   True
split :: Zipper -> Zipper
split (Zipper a l s r b) = Zipper (a |> l) "" s r b

-- | Removes a character from `left`
--
--   >>> backspace (paragraph "abc" "def" "ghi") == paragraph "ab" "def" "ghi"
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
--   delete (paragraph "" "A" "") == emptyZipper
--   True
--
--   >>> delete (paragraph "abc" "d" "efg") == paragraph "abc" "e" "fg"
--   True
--
--   >>> delete (paragraph "abc" "de" "fg") == paragraph "abc" "f" "g"
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
numbers = paragraph "1234" "5" "6789"
