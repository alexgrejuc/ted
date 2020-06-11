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

import Data.Text.Manipulate
import Data.Text.ICU (brkBreak, breakSentence, breaks)
import Data.Text.ICU.Types

type Position = (Int, Int)

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
fromLine :: Int -> Text -> Zipper
fromLine _ "" = emptyZipper
fromLine x t  = paragraph "" s r
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
fromText :: Position -> Text -> Zipper
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
--	  Obivated in editor by toText3 and only used for testing.
--
--   >>> let text = "1\n2\n3\n4\n" in let z = fromText (1,1) text in text == toText z
--   True
toText :: Zipper -> Text
toText (Zipper [] "" "" "" []) = ""
toText z = concatWith "\n" $ toLines z

parLen :: Zipper -> Int
parLen (Zipper _ l s r _) = T.length l + T.length s + T.length r

parLines :: Int -> Zipper -> Int
parLines w z = (roundUp (parLen z) w) `div` w

-- | Converts the visible portion of a Zipper into the text before the selection, the selection,
--   and the text after the selection
toText3 :: Int -> Int -> Int -> Zipper -> (Text, Text, Text)
toText3 w h o z@(Zipper a l s r b) =
   if T.length s >= w * h
   then ("", T.take (w * h) s, "")
   else (l' +++ remL, s, remR +++ r')
   where
      (sc, ec)   = cursorCol w z
      (sr, er)   = cursorRow w z
      remL       = T.takeEnd sc l
      fromTop    = sr - o
      l'         = concatS (takeTop w fromTop z)
      remR       = T.take ((w - ec - 1)) r
      fromBottom = h - (er + 1 - o)
      r'         = concatS (takeBottom w fromBottom z)

-- | Takes n lines of length w (or ending in a newline) from the text which precedes the first line 
--   of the current selection 
takeTop :: Int -> Int -> Zipper -> Seq Text
takeTop w n z@(Zipper a l _ _ _) = if n > 0 then takeTop' w n' a >< fromCurrentPar else []
   where
      -- first grab lines before the first line of the selection but in the same paragraph
      sc             = fst $ cursorCol w z
      (l', remL)     = (T.dropEnd sc l, T.takeEnd (sc) l)
      fromCurrentPar = S.fromList $ T.chunksOf w l'
      n'             = n - S.length fromCurrentPar
      
      -- then take from the preceding paragraphs 
      takeTop' w n [] = []
      takeTop' w n (xs :|> x) = if len >= n
                                then fromEnd
                                else takeTop' w (n - len) xs >< fromEnd
                                 where
                                    lins  = S.fromList (T.chunksOf w x)
                                    lins' = case lins of
                                             xs :|> x -> xs |> (x +++ "\n")
                                             _        -> ["\n"]
                                    len = S.length lins'
                                    fromEnd = S.reverse $ S.take n (S.reverse lins')

-- | Takes n lines of length w (or ending in a newline) from the text which follows the last line of 
--   the current selection 
takeBottom :: Int -> Int -> Zipper -> Seq Text
takeBottom w n z@(Zipper _ _ _ r b) = if n > 0
                                      then (fromCurrentPar |> "\n") >< takeBottom' w n' b 
                                      else []
   where
      -- first grab lines after the final line of the selection but in the same paragraph
      ec         = snd $ cursorCol w z
      (remR, r') = (T.take ((w - ec - 1)) r, T.drop ((w - ec - 1)) r)
      fromCurrentPar = S.fromList $ T.chunksOf w r'
      n'        = n - S.length fromCurrentPar
     
      -- then take from the following paragraphs 
      takeBottom' w n [] = []
      takeBottom' w n (x :<| xs) = if len >= n
                                    then fromStart
                                    else fromStart >< takeBottom' w (n - len) xs
                                       where
                                          lins = S.fromList (T.chunksOf w x)
                                          lins' = case lins of
                                             xs :|> x -> xs |> (x +++ "\n")
                                             _        -> ["\n"]
                                          len = S.length lins'
                                          fromStart = S.take n lins'
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
upPars :: Int -> Zipper -> Zipper
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
downPars :: Int -> Zipper -> Zipper
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
goLeft :: Int -> Zipper -> Zipper
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
goRight :: Int -> Zipper -> Zipper
goRight = appFix stepRight

-- | Performs a move on a zipper (row, column).
--   Vertically relative, horizontally relative to the x position after the vertical move.
go :: Int -> (Int, Int) -> Zipper -> Zipper
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
stepDown :: Int -> Zipper -> Zipper
stepDown len z@(Zipper a l s r b) = if atBottom then drop else wrap
   where
      llen       = len
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

goDown :: Int -> Int -> Zipper -> Zipper
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
stepUp :: Int -> Zipper -> Zipper
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
      llen = len

goUp :: Int -> Int -> Zipper -> Zipper
goUp n len z = appFix (stepUp len) n z

selectionPos :: Int -> Zipper -> (Position, Position)
selectionPos len z = ((fst row, fst col), (snd row, snd col))
   where
      row = cursorRow len z
      col = cursorCol len z

-- | Returns a 0-indexed (row, column) cursor position based on lines of length n
--
--   >>> cursorStart 10 emptyZipper == (0,0)
--   True
--
--   >>> cursorStart 10 $ makeZipper ["0", "1", "2", "3"] "01234" "5" "6789" ["below"]
--   (4,5)
--
--   >>> cursorStart 2 $ makeZipper ["0", "1", "2", "3"] "01234" "56" "6789" ["below"]
--   (6,1)
--
cursorStart :: Int -> Zipper -> Position
cursorStart len z = both fst (cursorRow len z, cursorCol len z)

-- | Returns a 0-indexed (row, column) cursor position based on lines of length n
--
--   >>> cursorEnd 10 emptyZipper == (0,0)
--   True
--
--   >>> cursorEnd 10 $ makeZipper ["0", "1", "2", "3"] "01234" "5" "6789" ["below"]
--   (4,5)
--
--   >>> cursorEnd 2 $ makeZipper ["0", "1", "2", "3"] "01234" "56" "789" ["below"]
--   (7,0)
--
cursorEnd :: Int -> Zipper -> Position
cursorEnd len z = both snd (cursorRow len z, cursorCol len z)

lsAbove :: Int -> Zipper -> Int
lsAbove len (Zipper a _ _ _ _) = prevChars `div` len
   where
      prevChars = foldr (\p sum -> (roundUp (T.length p) len + sum)) 0 a

-- | Returns a 0-indexed (colStart, colEnd) cursor position based on lines of length n
cursorCol :: Int -> Zipper -> Position
cursorCol len (Zipper a l s _ _) = (start, end)
   where
      onLeft = T.length l
      start  = onLeft `mod` len
      end    = (onLeft + ((T.length (T.drop 1 s)))) `mod` len

-- | Returns a 0-indexed (rowStart, rowEnd) cursor position based on lines of length n
cursorRow :: Int -> Zipper -> Position
cursorRow len (Zipper a l s _ _) = (start, end)
   where
      len' = len
      prevChars = foldr (\p sum -> (roundUp (T.length p) len' + sum)) 0 a
      linesAbove = prevChars `div` len'
      start = linesAbove + (T.length l `div` len')
      end = linesAbove + ((T.length l + T.length (T.drop 1 s)) `div` len')

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

-- | Appends a character to the left of the selection
appendChar :: Char -> Zipper -> Zipper
appendChar c z = z { left = snoc (left z) c }

-- | Appends text to the left of the selection
appendText :: Text -> Zipper -> Zipper
appendText t z = z { left = T.append (left z) t }

-- | Extends the cursor to include the sentence which encloses the current selection.
--   Does not handle acronyms well.
--
-- >>> selectSentence zSentences == zSentences'
-- True
selectSentence :: Zipper -> Zipper
selectSentence (Zipper a l s r b) = Zipper a l' (onLeft +++ s +++ onRight) r' b
   where
      (l', onLeft) = case T.breakOnEnd ". " l of
                        (before, start)        -> (T.dropEnd 1 before, T.takeEnd 1 before +++ start)
      (onRight, r') = case T.breakOn ". " r of
                        (end, "") -> (end, "")
                        (end, start) -> (end +++ ".", T.drop 1 start)

selectParagraph :: Zipper -> Zipper
selectParagraph z = z { left = "", selection = combineLine z, right = "" }

-- | Applies a function to the selection
toSelection :: (Text -> Text) -> Zipper -> Zipper
toSelection f z = z { selection = f (selection z) }

toUpper :: Zipper -> Zipper
toUpper = toSelection T.toUpper

toLower :: Zipper -> Zipper
toLower = toSelection T.toLower

sentences t = map ((\s -> (s, T.length s)) . brkBreak) (breaks (breakSentence Current) (T.pack t))

-- | Selects the text which is enclosed by some string s
selectEnclosed :: Text -> Zipper -> Zipper
selectEnclosed t (Zipper a l s r b) = Zipper a l' (onLeft +++ s +++ onRight) r' b
   where
      (l', onLeft) = T.breakOnEnd t l
      (onRight, r') = T.breakOn t r

selectWord :: Zipper -> Zipper
selectWord = selectEnclosed " "

posDiff :: Int -> Zipper -> Zipper -> (Int, Int)
posDiff l a b = pairDiff (cursorStart l a) (cursorEnd l b)

-- example zipper for testing and debugging:
numbers :: Zipper
numbers = paragraph "1234" "5" "6789"

zSentences = paragraph "A sentence. An" "o" "ther. Final."
zSentences' = paragraph "A sentence." " Another." " Final."

-- | A zipper which can be manipulated in ghci in case a reviewer has trouble building the project.
exampleZipper :: Zipper
exampleZipper = Zipper ["This is a paragraph above.", "", "This is another one."]
                       "This is text on the left." " " "This is text on the right."
                       ["This is a paragraph below.", "", "This is more text"]

ez = Zipper ["abcde", "abcde"] "abc1234" "34" "5" ["abcde", "abcde"]
