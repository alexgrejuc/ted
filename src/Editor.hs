{-|
Module      : Editor
Description : IO manipulation of a Text Zipper and corresponding display changes

The code for mapping user actions via keyboard to operations on the zipper and to the display.
-}

{-# LANGUAGE ViewPatterns, PatternSynonyms, OverloadedLists, OverloadedStrings #-}
module Editor where

import UI.NCurses

import Zipper
import Lib

import Data.Text (Text)
import qualified Data.Text as T

import qualified Data.Text.IO as I

import Data.Sequence (Seq, pattern Empty, pattern (:<|), pattern (:|>))
import Data.Sequence as S

import Control.Monad.State

import Debug.Trace

import System.Exit

data Mode = Insert | Edit
   deriving (Show, Eq)

data TedState =
   TedState { mode   :: Mode
            , offset :: Integer
            , text   :: Zipper
            , path   :: String
            }

type MonadEditor a = StateT TedState Curses a

runEditor :: TedState -> Curses ()
runEditor = evalStateT $ do
   w <- lift defaultWindow
   tedRender w id
   eventLoop

eventLoop :: MonadEditor ()
eventLoop = do
      w  <- lift defaultWindow
      ev <- lift (getEvent w Nothing)
      case ev of
         Nothing  -> eventLoop
         Just ev' -> do
                        handleEvent w ev'
                        eventLoop

handleEvent :: Window -> Event -> MonadEditor ()
handleEvent w (EventCharacter '\ESC') = modify (\s -> s { mode = toggleMode (mode s) })
handleEvent w e = do
                     s <- get
                     if mode s == Insert then insertEvent w e else editEvent w e

-- | Handles an edit mode event by making the corresponding zipper update and displaying it
editEvent :: Window -> Event -> MonadEditor ()
editEvent w (EventCharacter 's')   = tedRender w (delete . selectSentence)
editEvent w e@(EventSpecialKey k)  = if isArrowKey k then insertEvent w e else return ()
editEvent w (EventCharacter 'q')   = do
                                        s <- get
                                        let z = text s
                                        liftIO $ write (path s ++ ".ted") z
                                        liftIO exitSuccess
editEvent w e                      = return () -- ignore everything else (e.g. mouse clicks)

-- | Handles an insert mode event by making the corresponding zipper update and displaying it
insertEvent :: Window -> Event -> MonadEditor ()
insertEvent w (EventSpecialKey k)    = do { (_, l) <- lift screenSize; tedRender w (act (l - 1) k) }
insertEvent w (EventCharacter '\n')  = tedRender w split
insertEvent w (EventCharacter c)     = tedRender w (appendChar c)
insertEvent w e                      = return () -- ignore everything else (e.g. mouse clicks)

-- | Maps a key press to the corresponding operation on the zipper
act :: Integer -> Key -> Zipper -> Zipper
act l k z
   | isArrowKey            k = go l (arrowToMove l z k) z
   | KeyBackspace       == k = backspace z
   | KeyDeleteCharacter == k = delete z
   | otherwise               = z

-- | Draws a zipper to the terminal. Skips offset o lines from the beginning of the zipper
draw :: Integer -> Zipper -> Curses ()
draw o z = do
            selectionColor <- newColorID ColorRed ColorBlack 5
            w <- defaultWindow
            (lines, cols) <- screenSize
            let (r, c) = cursorStart (cols - 1) z
            let rows = S.take (fromIntegral lines) (S.drop (fromIntegral o) (toRows (cols - 1) z))
            updateWindow w (do
                              setColor defaultColorID
                              draw' selectionColor rows 0
                              moveCursor (r - o) c
                           )
   where
      draw' c []     _   = return ()
      draw' c [x]    r   = moveCursor r 0 >> drawText x >> clearLine
      draw' c (x:<|xs) r = do
                           moveCursor r 0
                           drawText x
                           clearLine
                           draw' c xs (r + 1)

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
      lift $ draw off z'
      lift render
      modify (\s -> s { offset = off, text = z' })

-- | Converts an arrow key press to the (y,x) go-relative coordinates.
arrowToMove :: Integer -> Zipper -> Key -> (Integer, Integer)
arrowToMove l z k = let (_, c) = cursorStart l z in convert c k
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

write :: FilePath -> Zipper -> IO ()
write p z = do
               I.writeFile p ""
               mapM_ af (above z)
               af (combineLine z)
               mapM_ af (below z)
       where
         af = I.appendFile p . flip T.append "\n"
