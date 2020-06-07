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
   runCurses $ do
           setEcho False                    -- displaying to terminal handled manually
           let zipper = fromText (0, 0) text
           runEditor $ TedState { mode = Insert, offset = 0, text = zipper, path = path }

-- | Returns a file path based on the command line argument and returns that path along with empty
--   Text or the contents of the file, if it exists.
start :: IO (String, Text)
start = do
   args <- getArgs
   case args of
      [path] -> do
         exists <- doesFileExist path
         if exists
            then do
               text <- I.readFile path
               return (path, text)
            else return (path, "")
      _ -> do
         putStrLn $ "Must enter exactly 1 argument. You entered " ++ (show (length args))
         exitFailure
