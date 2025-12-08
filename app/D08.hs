{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Data.Attoparsec.ByteString (Parser, sepBy)
import Data.Attoparsec.ByteString.Char8 (char8, decimal, space)

import Data.List (sortBy, sortOn)
import qualified Data.List as List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Debug.Trace as Debug
import Util (run)

main :: IO ()
main = run 2025 8 parser parser part1 part2
type Input = [Point]

data Point = Point Int Int Int
instance Eq Point where
    p == q = coords p == coords q
instance Ord Point where
    compare p q = compare (coords p) (coords q)
instance Show Point where
    show (Point x y z) = show (x, y, z)

parser :: Parser Input
parser = point `sepBy` space
  where
    point :: Parser Point
    point = Point <$> decimal <* char8 ',' <*> decimal <* char8 ',' <*> decimal

part1 :: Input -> Int
part1 =
    product
        . take 3
        . sortBy (flip compare)
        . map Set.size
        . subGraphs
        . edgesFromList
        . concatMap bidirectionalise
        . take 1000
        . map discardThird
        . sortOn third
        . connect

part2 :: Input -> Int
part2 points = x * x'
  where
    (Point x _ _, Point x' _ _) =
        ( connectInOrder (length points)
            . map discardThird
            . sortOn third
            . connect
        )
            points

distance :: Point -> Point -> Int
distance p q = (sum . map sq) (zipWith (-) (coords p) (coords q))
  where
    sq x = x * x

coords :: Point -> [Int]
coords (Point x y z) = [x, y, z]

connect :: [Point] -> [(Point, Point, Int)]
connect = concat . mapWithTail connections

connections :: Point -> [Point] -> [(Point, Point, Int)]
connections p = map (\q -> (p, q, distance p q))

mapWithTail :: (Show a) => (a -> [a] -> b) -> [a] -> [b]
mapWithTail _ [] = []
mapWithTail f (x : xs) = f x xs : (mapWithTail f xs)

bidirectionalise :: (a, a) -> [(a, a)]
bidirectionalise (x, y) = [(x, y), (y, x)]

third :: (a, b, c) -> c
third (_, _, z) = z

discardThird :: (a, b, c) -> (a, b)
discardThird (x, y, _) = (x, y)

data Edges v = Edges (Map v (Set v))
instance (Show v) => Show (Edges v) where
    show (Edges e) = "Edges " <> (show e)

edgesFromList :: (Ord v) => [(v, v)] -> Edges v
edgesFromList = Edges . foldr insert Map.empty
  where
    insert (p, q) = Map.insertWith (<>) p (Set.singleton q)

takeEdges :: (Ord v) => v -> Edges v -> (Set v, Edges v)
takeEdges from (Edges edges) = (vertices, Edges edges')
  where
    vertices = fromMaybe Set.empty (edges Map.!? from)
    edges' = Map.delete from edges

subGraphs :: (Ord v, Show v) => Edges v -> [Set v]
subGraphs edges@(Edges es) = case (Map.keys es) of
    (start : _) -> let (g, edges') = subGraph start edges in g : subGraphs edges'
    _ -> []

subGraph :: (Ord v, Show v) => v -> Edges v -> (Set v, Edges v)
subGraph start = let start' = Set.singleton start in expand start' start'
  where
    expand :: (Ord v, Show v) => Set v -> Set v -> Edges v -> (Set v, Edges v)
    expand collected active edges = case Set.minView (active) of
        Just (next, active') ->
            let (vertices, edges') = takeEdges next edges
             in expand (vertices <> collected) (vertices <> active') edges'
        Nothing -> (collected, edges)

connectInOrder :: (Ord v, Show v) => Int -> [(v, v)] -> (v, v)
connectInOrder = go []
  where
    go :: (Ord v, Show v) => [Set v] -> Int -> [(v, v)] -> (v, v)
    go gs _ [] = Debug.trace (show gs) $ error "subgraphs are disjoint"
    go gs n (e : es) = case addEdgeBidi e gs of
        [g] | n == (Set.size g) -> Debug.trace (show g) e
        gs' -> go gs' n es
    addEdgeBidi :: (Ord v) => (v, v) -> [Set v] -> [Set v]
    addEdgeBidi (x, y) = addEdge (y, x) . addEdge (x, y)
    addEdge :: (Ord v) => (v, v) -> [Set v] -> [Set v]
    addEdge (from, to) gs = merged : notFound
      where
        merged = foldr (<>) (Set.singleton to) found
        (found, notFound) = List.partition (from `Set.member`) gs
