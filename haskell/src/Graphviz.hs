module Graphviz where

import PERT

type Document = [(String, (Range, Range), [String])]

writeGraph :: Document -> IO ()
writeGraph doc = putStrLn $ convert doc
  where
    convert doc = unlines $ concatMap ($ doc) [header, nodes, edges, footer]
    --header, nodes, edges, footer :: Document -> [String]
    header _ = ["digraph G {", "  rankdir=LR;", ""]
    nodes = map node
    edges = ("":) . concatMap edge
    footer _ = ["}"]

    cp = criticalPath [ (name, time) | (name, time, _) <- doc ]
    node (name, times, _) = "  \"" ++ name ++ "\" [shape=rect,label=" ++ label times ++ cpStyle name ++ "]"
    label ((es, ef), (ls, lf)) | es == ls && ef == lf =
      "<<TABLE BORDER=\"0\"><TR>" ++
      "<TD>" ++ show es ++ "</TD>" ++
      "<TD>\\N</TD>" ++
      "<TD>" ++ show ef ++ "</TD>" ++
      "</TR></TABLE>>"
    label ((es, ef), (ls, lf)) | otherwise =
      "<<TABLE BORDER=\"0\"><TR>" ++
      "<TD>" ++ show es ++ "</TD>" ++
      "<TD></TD>" ++
      "<TD>" ++ show ef ++ "</TD></TR>" ++
      "<TR><TD></TD><TD>\\N</TD><TD></TD></TR>" ++
      "<TR>" ++
      "<TD>" ++ show ls ++ "</TD>" ++
      "<TD></TD>" ++ 
      "<TD>" ++ show lf ++ "</TD>" ++
      "</TR></TABLE>>"
      
    edge (name, _, deps) = [ "  \"" ++ dep ++ "\" -> \"" ++ name ++ "\"" ++ edgeLabel name dep ++ ";" | dep <- deps ]

    cpStyle n | n `elem` cp = ",style=bold"
              | otherwise   = ""
    
    edgeLabel e f | e `elem` cp && f `elem` cp = "[style=bold]"
                  | otherwise                  = ""
