{-# LANGUAGE ScopedTypeVariables #-}

module PERT (pert, criticalPath) where

--import Data.List.Split
--import qualified Data.ByteString.Lazy as BL
--import qualified Data.Vector as V
--import Data.Csv
--import Debug.Trace
import Data.Graph.Inductive
import qualified Data.Map as M
import Data.Maybe (fromJust)

type ActivityNode a = (a, Double) 

type Range = (Double, Double)

get :: Ord k => M.Map k a -> k -> a
get m k = fromJust $ M.lookup k m

-- | Calculate PERT for a given graph. The input should be a list of
-- tuples shaped like (entity, duration, dependencies).
pert :: Ord a => [(a, Double, [a])] -> [(a, (Range, Range))]
pert = pertGraph . toGraph

-- | Convert a list of tuples (entity, duration, dependencies) into a
-- graph of activity nodes.
toGraph :: Ord a => [(a, Double, [a])] -> Gr (ActivityNode a) ()
toGraph acts = mkGraph nodes edges
  where
    nodeNames = M.fromList $ zipWith (\(l,_,_) x -> (l,x)) acts [1..]
    nodes     = zipWith (\(l,c,_) x -> (x,(l,c))) acts [1..]
    edges     = [ (get nodeNames dep, get nodeNames l, ())
                | (l,_,deps) <- acts, dep <- deps ]

-- | Calculate the early start/finish times for a PERT graph.
early :: Gr (ActivityNode a) () -> [(Node, Range)]
early g = M.toList $ foldl calc M.empty (bfs 1 g)
  where
    calc prev node = M.insert node (prevCost, (prevCost + myCost)) prev
      where
        ctx         = context g node
        (_, myCost) = lab' ctx
        prevCost    = if null (pre' ctx) then 0
                      else foldr1 max $ map (snd . get prev) $ pre' ctx

-- | Calculate the late start/finish times for a PERT graph.
late :: Double -> Gr (ActivityNode a) () -> [(Node, Range)]
late latestFinish g = M.toList $ foldr calc M.empty (bfs 1 g)
  where
    calc node next = M.insert node ((nextCost - myCost), nextCost) next
      where
        ctx         = context g node
        (_, myCost) = lab' ctx
        nextCost    = if null (suc' ctx) then latestFinish
                      else foldr1 min $ map (fst . get next) $ suc' ctx

-- | Calculates PERT for the given graph. Returns a list of tuples
-- shaped like (entity, 
--               ((early start, early finish),
--                (late  start, late  finish)))
pertGraph :: Gr (ActivityNode a) () -> [(a, (Range, Range))]
pertGraph g = zipWith (\(n, (es, ef)) (_, (ls, lf)) ->
                        (label n, ((es, ef), (ls, lf))))
              early'
              late'
  where
    early' = early g
    latest = snd $ snd $ last early'
    late'  = late latest g
    label  = fst . fromJust . lab g

-- | Calculate the critical path given PERT output.
criticalPath :: [(a, (Range, Range))] -> [a]
criticalPath = map fst . filter (\(_, ((a,b),(c,d))) -> a == c && b == d)
