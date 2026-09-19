module Main where

import LGPT.Helpers
import LGPT.TUI qualified as TUI
import Control.Monad.State
import System.Directory
import System.FilePath
import System.IO (readFile')
import qualified Data.Map as Map

{- | This is what gets run when you run the program. 

    It just calls the runREPL function in TUI.hs, which is where the real work 
    happens :)
-}
main :: IO ()
main = do
  -- Pre-initialisation to set up the terminal
  runStart

  -- Memory checking
  fileExists <- doesFileExist "src/LGPT/.memory.txt"

  if fileExists 
    then do
        -- Load memory
        fileContents <- readFile' "src/LGPT/.memory.txt"
        -- Run loop
        evalStateT TUI.runREPL (read fileContents)
    else 
        -- Run loop with empty memory
        evalStateT TUI.runREPL Map.empty

