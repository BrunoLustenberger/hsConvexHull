{- |
Module : Graph
Description : Creates a graph of a point set and its convex hull.
Copyright : (c) 2022 Bruno M.S. Lustenberger
The point set and the convex hull are read from text files,
which must obey the "point list syntax", see readme.md.
The generated graph is stored in a PNG file.

Actually, the two text files need not represent a point set and
its convex hull, they can represent any point sets. The first file
is just displayed as a point set and the second as a closed 
connected path of points. 

You can compile this module into a main program like so:
$ ghc -main-is Graph.doPlot Graph.hs

Then you can call this program e.g. like so:
$ ./graph "set1.txt" "set2.txt" "graph.png"

Alternatively, the function generate can be imported 
in another module and called to generate a graph.
-}
module Graph (
      generate
    , doPlot
    ) where

import System.IO (FilePath, readFile, writeFile)
import System.Environment (getArgs)
import Graphics.Gnuplot.Simple

import Utils (stringToIntPairList)

setStyle = PlotStyle 
    { plotType = Points,
      lineSpec = CustomStyle [LineTitle "Points not part of hull",
                              PointType 7,
                              PointSize 1.5]}

hullStyle = PlotStyle 
    { plotType = LinesPoints,
      lineSpec = CustomStyle [LineTitle "Hull",
                              PointType 7,
                              PointSize 1.5,
                              LineWidth 2]}


generate :: FilePath -> FilePath -> FilePath -> IO ()
generate inFile1 inFile2 outFile = do
    inStr1 <- readFile inFile1
    inStr2 <- readFile inFile2
    let set1 = stringToIntPairList inStr1
    let set2 = stringToIntPairList inStr2
    plotPathsStyle 
        [PNG outFile] 
        [ (setStyle, set1)
        , (hullStyle, set2 ++ [head set2]) -- ++ head to get a closed path
        ]

-- |Main program. The paths of the two input files and the output file
-- are passed as commandline arguments.
doPlot :: IO ()
doPlot = do
    args <- getArgs
    generate (args !! 0) (args !! 1) (args !! 2)


