
{- |
Module : Main
Description : Compute convex hull, input/output is given by files.
Copyright : (c) 2021 Bruno M.S. Lustenberger
Reads a point list from an input file computes its convex hull
and stores it as a point list in an output file.
See readme for the syntax of a point list.
-}
module Main (
      processFiles
    , main
    ) where

import System.IO (FilePath, readFile, writeFile)
import System.Environment (getArgs)

import ConvexHull (processInputString)

-- |Process the files given by their path.
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
