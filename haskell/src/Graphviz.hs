module Graphviz where

import PERT

type Document = [(String, (Range, Range), [String])]

writeGraph :: Document -> IO ()
writeGraph doc = putStrLn $ convert doc
  where
    convert doc = unlines $ concatMap ($ doc) [header, nodes, edges, footer]
    header, nodes, edges, footer :: Document -> [String]
    header _ = ["digraph G {", ""]
    nodes = map node
    edges = ("":) . concatMap edge
    footer _ = ["", "}"]
    node (name, ((es, ef), (ls, lf)), deps) = "  \"" ++ name ++ "\" [label=\"" ++ label ++ "\"]"
    label = "hi"
    edge (name, _, deps) = [ "  \"" ++ dep ++ "\" -> \"" ++ name ++ "\";" | dep <- deps ]
