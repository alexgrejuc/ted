{-|
Module      : Editor
Description : IO manipulation of a Text Zipper and corresponding display changes

The code for mapping user actions via keyboard to operations on the zipper and to the display.
-}

{-# LANGUAGE ViewPatterns, PatternSynonyms, OverloadedLists, OverloadedStrings #-}
module Editor where

import UI.NCurses

import Zipper

import Data.Text (Text)
import qualified Data.Text as T

import qualified Data.Text.IO as I

import Data.Sequence (Seq, pattern Empty, pattern (:<|), pattern (:|>))
import Data.Sequence as S

import Control.Monad.State

import Debug.Trace
data Mode = Insert | Edit
   deriving (Show, Eq)

data TedState =
   TedState { mode   :: Mode
            , offset :: Integer
            , text   :: Zipper
            , path   :: String
            }

type MonadEditor a = StateT TedState Curses a

startEditor = runStateT $ do
   w <- lift defaultWindow
   tedRender w id
   eventLoop

-- | Runs the editor
eventLoop :: MonadEditor ()
eventLoop = do
      s <- get
      let m = mode s
          z = text s
      w <- lift defaultWindow
      ev <- lift (getEvent w Nothing)
      (_, l) <- lift screenSize
      case ev of
         Nothing -> eventLoop
         Just (EventCharacter '\ESC') ->
            do
               let output = T.append (toText z) (T.pack ("\n\n" ++ show z ++ show (cursorPos l z)))
               liftIO $ I.writeFile (path s ++ ".ted") output -- temp, to prevent overwriting files
               liftIO $ putStrLn ("saved copy of file to " ++ path s ++ ".ted")
         Just ev' -> do
                        handleEvent w ev'
                        eventLoop

pairDiff :: (Num a, Num b) => (a, b) -> (a, b) -> (a, b)
pairDiff l r = (fst r - fst l, snd r - snd l)

posDiff :: Integer -> Zipper -> Zipper -> (Integer, Integer)
posDiff l a b = pairDiff (cursorPos l a) (cursorPos l b)

-- | Handles an event by making the corresponding zipper update and displaying it
handleEvent :: Window -> Event -> MonadEditor ()
handleEvent w (EventSpecialKey k)    = do { (_, l) <- lift screenSize; tedRender w (act (l - 1) k) }
handleEvent w (EventCharacter '\n')  = tedRender w split
handleEvent w (EventCharacter c)     = tedRender w (appendChar c)
handleEvent w e                      = return () -- ignore everything else (e.g. mouse clicks)

-- | Maps a key press to the corresponding operation on the zipper
act :: Integer -> Key -> Zipper -> Zipper
act l k z
   | isArrowKey            k = go l (arrowToMove l z k) z
   | KeyBackspace       == k = backspace z
   | KeyDeleteCharacter == k = delete z
   | KeyEnter           == k = split z -- TODO: why does Enter get interpreted as '\n'?
   | otherwise               = z

draw :: Zipper -> Integer -> Curses ()
draw z o = do
            w <- defaultWindow
            (lines, cols) <- screenSize
            let (r, c) = cursorPos (cols - 1) z
            let rows = S.take (fromIntegral lines) (S.drop (fromIntegral o) (toRows (cols - 1) z))
            updateWindow w (do
                              clear
                              draw' rows 0
                              moveCursor (r - o) c
                           )
   where
      draw' []     _   = return ()
      draw' [x]    r   = moveCursor r 0 >> drawText x
      draw' (x:<|xs) r = do
                           moveCursor r 0
                           drawText x
                           draw' xs (r + 1)

-- | Updates the display based on the state of the zipper.
tedRender :: Window -> (Zipper -> Zipper) -> MonadEditor ()
tedRender w f = do
      s <- get
      (r, _) <- lift $ getCursor w
      (nr, nc) <- lift $ screenSize
      let z   = text s
          z'  = f z
          (dr, _) = posDiff (nc - 1) z z'
          offdiff = if r + dr >= nr || r + dr < 0 then dr else 0
          off     = offset s + offdiff
      lift $ draw z' off
      lift render
      modify (\s -> s { offset = off, text = z' })

-- | Converts an arrow key press to the (y,x) go-relative coordinates.
arrowToMove :: Integer -> Zipper -> Key -> (Integer, Integer)
arrowToMove l z k = let (_, c) = cursorPos l z in convert c k
   where
      convert c KeyUpArrow    = (-1, 0)
      convert c KeyDownArrow  = (1, 0)
      convert _ KeyLeftArrow  = (0, -1)
      convert _ KeyRightArrow = (0, 1)

arrowKeys :: [Key]
arrowKeys = [KeyUpArrow, KeyDownArrow, KeyLeftArrow, KeyRightArrow]

isArrowKey :: Key -> Bool
isArrowKey k = elem k arrowKeys

toggleMode :: Mode -> Mode
toggleMode Insert = Edit
toggleMode Edit   = Insert

toggleModeKey :: Event
toggleModeKey = EventCharacter '\ESC'
