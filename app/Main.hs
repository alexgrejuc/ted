{-|
Module      : Main
Description : The high level file IO component of the text editor.
-}

{-# LANGUAGE OverloadedStrings #-}
module Main where

import UI.NCurses

import System.Environment
import System.Directory
import System.Exit

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as I

import Zipper
import Editor

-- | Edits a buffer created from the user-specified path until a user chooses to quit and then saves
--   it to a file.
main :: IO ()
main = do
   (path, text) <- start
   z <- runCurses $ do
           setEcho False                    -- displaying to terminal handled manually
           w <- defaultWindow
           updateWindow w $ do
              drawText text
              moveCursor 0 0
           render
           let zipper = fromText (0, 0) text
           run w Insert zipper
   let output = T.append (toText z) (T.pack ("\n\n" ++ show z))
   I.writeFile (path ++ ".ted") output -- temporary, to prevent overwriting files
   putStrLn $ "saved copy of file to " ++ path ++ ".ted"

-- | Returns a file path based on the command line argument and returns that path along with empty
--   Text or the contents of the file, if it exists.
start :: IO (String, Text)
start = do
   args <- getArgs
   if length args == 1
      then let path = head args in do
         exists <- doesFileExist path
         if exists
            then do
               text <- I.readFile path
               return (path, text)
            else return (path, "")
      else do
         putStrLn $ "Must enter exactly 1 argument. You entered " ++ (show (length args))
         exitFailure
