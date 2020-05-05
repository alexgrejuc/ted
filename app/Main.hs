module Main where
import UI.NCurses
import System.Environment
import System.Directory
import System.Exit

start :: IO String
start = do
   args <- getArgs
   if length args == 1
      then let path = head args in do
         exists <- doesFileExist path
         if exists
            then readFile path
            else return ""
      else do
         putStrLn $ "Must enter exactly 1 argument. You entered " ++ (show (length args))
         exitFailure

main :: IO ()
main = do
   s <- start
   runCurses $ do
      setEcho True
      w <- defaultWindow
      updateWindow w $ do
         drawString s
         moveCursor 0 0
      render
      waitFor w (\ev -> ev == EventCharacter 'q' || ev == EventCharacter 'Q')

waitFor :: Window -> (Event -> Bool) -> Curses ()
waitFor w p = loop where
    loop = do
        ev <- getEvent w Nothing
        case ev of
            Nothing -> loop
            Just ev' -> if p ev' then return () else loop

--import Lib
--
--main :: IO ()
--main = someFunc
