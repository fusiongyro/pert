{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Data.List.Split
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V
import Data.Csv
import Debug.Trace
import Data.Graph.Inductive
import qualified Data.Map as M
import Data.Maybe

type ActivityNode = (String, Double)
type Range = (Double, Double)

type Activity = (String, Double, [String])

parseActivitiesFile :: FilePath -> IO [Activity]
parseActivitiesFile fp = do
  csvData <- BL.readFile fp
  case decode False csvData of
    Left err -> fail $ "Parse error: " ++ show err
    Right v -> return $ map convert (V.toList v)
      where
        convert (name, time :: Double, deps) = (name, time, split (dropBlanks $ oneOf ",") deps)

sampleGraph :: Gr ActivityNode ()
sampleGraph = mkGraph
              [ (1, ("Fuselage", 20))
              , (2, ("Wings", 38))
              , (3, ("Tail", 22))
              , (4, ("Landing Gear", 28))
              , (5, ("Horizontal Stabilizer", 2))
              , (6, ("Vertical Stabilizer", 2))
              , (7, ("Wheels", 8))
              , (8, ("Airfoil", 8))]
              [ (1,2, ())
              , (1,3, ())
              , (1,4, ())
              , (3,5, ())
              , (3,6, ())
              , (4,7, ())
              , (2,8, ())]

activities :: [Activity]
activities = [ ("Fuselage", 			20, [])
             , ("Wings", 			38, ["Fuselage"])
             , ("Tail", 			22, ["Fuselage"])
             , ("Landing Gear", 		28, ["Fuselage"])
             , ("Horizontal Stabilizer",	 2, ["Tail"])
             , ("Vertical Stabilizer", 		 2, ["Tail"])
             , ("Wheels", 			 8, ["Landing Gear"])
             , ("Airfoil", 			 8, ["Wings"])]

activitiesToGraph :: [Activity] -> Gr ActivityNode ()
activitiesToGraph acts = mkGraph nodes edges
  where
    nodeNames = M.fromList $ zipWith (\(l,_,_) x -> (l,x)) acts [1..]
    nodes     = zipWith (\(l,c,_) x -> (x,(l,c))) acts [1..]
    edges     = [ (get nodeNames dep, get nodeNames l, ())
                | (l,_,deps) <- activities, dep <- deps ]

get :: Ord k => M.Map k a -> k -> a
get m k = fromJust $ M.lookup k m

early :: Gr ActivityNode () -> [(Node, Range)]
early g = M.toList $ foldl calc M.empty (bfs 1 g)
  where
    calc prev node = M.insert node (prevCost, (prevCost + myCost)) prev
      where
        ctx         = context g node
        (_, myCost) = lab' ctx
        prevCost    = if null (pre' ctx) then 0
                      else foldr1 max $ map (snd . get prev) $ pre' ctx

late :: Double -> Gr ActivityNode () -> [(Node, Range)]
late latestFinish g = M.toList $ foldr calc M.empty (bfs 1 g)
  where
    calc node next = M.insert node ((nextCost - myCost), nextCost) next
      where
        ctx         = context g node
        (_, myCost) = lab' ctx
        nextCost    = if null (suc' ctx) then latestFinish
                      else foldr1 min $ map (fst . get next) $ suc' ctx

pert :: Gr ActivityNode () -> [(String, (Range, Range))]
pert g = zipWith (\(n, (es, ef)) (_, (ls, lf)) ->
                       (label n, ((es, ef), (ls, lf))))
             early'
             late'
  where
    early' = early g
    latest = snd $ snd $ last early'
    late'  = late latest g
    label  = fst . fromJust . lab g

criticalPath :: [(String, (Range, Range))] -> [String]
criticalPath = map fst . filter (\(_, ((a,b),(c,d))) -> a == c && b == d)

main = do
  activities <- parseActivitiesFile "example.csv"
  let sampleGraph' = activitiesToGraph activities
  putStrLn $ "The sample graph is: " ++ (show sampleGraph')

  let pert' = pert sampleGraph'
  
  putStrLn $ "PERT produces: " ++ (show pert')
  putStrLn $ "The critical path is: " ++ (show $ criticalPath pert')

