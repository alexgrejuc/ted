{-|
Module      : Editor
Description : IO manipulation of a Text Zipper and corresponding display changes

The code for mapping user actions via keyboard to operations on the zipper and to the display.
-}

{-# LANGUAGE OverloadedStrings #-}
module Editor where

import UI.NCurses

import Zipper

import Data.Text (Text)
import qualified Data.Text as T

import Debug.Trace

data Mode = Insert | Edit
   deriving (Show, Eq)

-- | Runs the editor
run :: Window -> Mode -> Zipper -> Curses Zipper
run  w m z = do
      ev <- getEvent w Nothing
      case ev of
         Nothing -> run w m z
         Just (EventCharacter '\ESC') -> return z -- TODO: temporary until edit mode is implemented
         Just ev' -> do
                        z' <- handleEvent w z ev'
                        run w m z'

-- | Handles an event by making the corresponding zipper update and displaying it
handleEvent :: Window -> Zipper -> Event -> Curses Zipper
handleEvent w z (EventSpecialKey k)    = tedRender w (act k z)
handleEvent w z (EventCharacter '\n')  = tedRender w (split z)
handleEvent w z (EventCharacter c)     = tedRender w (appendChar c z)
handleEvent w z e                      = return z -- ignore everything else (e.g. mouse clicks)

-- | Maps a key press to the corresponding operation on the zipper
act :: Key -> Zipper -> Zipper
act k z
   | isArrowKey            k = go (arrowToMove z k) z
   | KeyBackspace       == k = backspace z
   | KeyDeleteCharacter == k = delete z
   | KeyEnter           == k = split z -- TODO: why does Enter get interpreted as '\n'?
   | otherwise               = z

-- | Updates the display based on the state of the zipper.
tedRender :: Window -> Zipper -> Curses Zipper
tedRender w z = do
      let (r, c) = cursorPos z
      updateWindow w (do
                        clear
                        moveCursor 0 0
                        drawText (toText z)
                        moveCursor r c
                      )
      render
      return z

-- | Converts an arrow key press to the (y,x) go-relative coordinates.
arrowToMove :: Zipper -> Key -> (Integer, Integer)
arrowToMove z k = let (_, c) = cursorPos z in convert c k
   where
      convert c KeyUpArrow    = (-1, c)
      convert c KeyDownArrow  = (1, c)
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
