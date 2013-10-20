module Main where

import System.Environment

import Parser
import PERT
import Graphviz

main = do
  args <- getArgs
  let (filename:_) = args
  activities <- parseFile filename
  let pert' = pert activities
  let graph = zipWith (\(name, _, deps) (_, times) -> (name, times, deps)) activities pert'
  writeGraph graph
