
{- |
Module : Main
Description : Compute convex hull, input/output is given by files.
Copyright : (c) 2021 Bruno M.S. Lustenberger

Reads a point list from an input file, computes its convex hull
and stores it as a point list in an output file.
See readme for the syntax of a point list.
Additionally, creates a graph of the points and their convex hull.

You can compile this module into a main program like so:
$ ghc main.hs
Then you can call this program e.g. like so:
$ ./main "points.txt" "points.out.txt" "points.png"
Here, the file points.txt contains the points, the file
points.out.txt contains the points making up the convex hull
and the file points.png contains the graph.

Alternatively, you can load this module in ghci and call
the function processFiles directly.

-}
module Main (
      processFiles
    , main
    ) where

import System.IO (FilePath, readFile, writeFile)
import System.Environment (getArgs)

import ConvexHull (convexHull)
import Utils (ptListToString, stringToPtList)
import qualified Graph

-- Apply convexHull function on input given as string
-- and return result as string.
processStrings :: String -> String
processStrings = ptListToString . convexHull . stringToPtList

-- |Process the files given by their path.
processFiles :: FilePath -> FilePath  -> FilePath -> IO ()
processFiles inFilePath outFilePath graphFilePath = do
    inStr <- readFile inFilePath
    let outStr = processStrings inStr
    writeFile outFilePath outStr
    Graph.generate inFilePath outFilePath graphFilePath

-- |Main program. The paths of input and output files are 
--  passed as commandline arguments.
main :: IO ()
main = do
    args <- getArgs
    processFiles (args !! 0) (args !! 1) (args !! 2)
