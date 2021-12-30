
{- |
Module : Main
Description : Simple example of IO with files containing strings.
Copyright : (c) 2021 Bruno M.S. Lustenberger
You can use this module in 2 ways:
1.  Load it in ghci and call the function processFiles, e.g.
        ghci>processFiles "myInput.txt" "myOutput.txt"
2.  Compile this module using ghc and call the generated exe program, e.g.
        $ ghc main
        $ main "myInput.txt" "output.txt"
    Note: check with 
        $ which
    where the command main points to. It could point to some other location,
    in this case use ./main.
        $ which main                                                
        /Users/Ls/.local/bin/main
        $ ./main "myInput.txt" "myOutput.txt"
-}
module Main (
      processFiles
    , main
    ) where

import System.IO (FilePath, readFile, writeFile)
import Data.Char (toUpper)
import System.Environment (getArgs)

-- The actual computation, input and output are strings.
-- From this function on "inwards" everything is pure.
-- Typically, this function would be imported from a pure module.
processInputString :: String -> String
processInputString s = map toUpper s

-- |Reads the entire contents of a an input file into a string,
--  processes that string and writes the entire result as a string
--  to an output file. The files are given by their path.
processFiles :: FilePath -> FilePath -> IO ()
processFiles inFilePath outFilePath = do
    inStr <- readFile inFilePath
    let outStr = processInputString inStr
    writeFile outFilePath outStr

-- |Main program. The paths of input and output files are 
--  passed as commandline arguments.
main :: IO ()
main = do
    args <- getArgs
    processFiles (args !! 0) (args !! 1)
