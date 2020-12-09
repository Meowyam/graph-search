{-# OPTIONS_GHC -XFlexibleInstances #-}

module Main where

import Data.Maybe
import Data.Text.Lazy as T (pack)

import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree

import Data.GraphViz
import Data.GraphViz.Printing
import Data.GraphViz.Commands
import Data.GraphViz.Attributes.Complete

import Data.List
import Data.Tuple

smallGrNodes :: [LNode String]
smallGrNodes = [(0, "A"), (1, "B"), (2, "C"), (3, "G")]

smallGrEdges :: [LEdge Int]
smallGrEdges = [(0, 1, 3), (1, 2, 4), (1, 3, 2), (2, 3, 5), (3, 2, 5)]

data EdgeListGraph a = ELG [a] [(a, a)]
  deriving (Eq, Ord, Show, Read)

data NextFunGraph a = NFG [a] (a -> [a])

edgeList :: [[Char]]
edgeList = ["A", "B", "C", "G"]

smallGrELG =
  ELG ["A", "B", "C", "G"] [("A", "B"), ("B", "C"), ("B", "G"), ("C", "G"), ("G", "C")]

smallGrNexts x = case x of
  "A" -> ["B"]
  "B" -> ["C", "G"]
  "C" -> ["G"]
  "G" -> ["C"]
  _ -> []

smallGrNFG = NFG ["A", "B", "C", "G"] smallGrNexts

edgeListGraphToGrNodes :: [a] -> [LNode a]
edgeListGraphToGrNodes = zip =<< enumFromTo 0 . length

mapTheTuples :: Eq a => (a, a) -> [a] -> LEdge String
mapTheTuples a li = (l,r,"")
  where
    l = go (fst a)
    r = go (snd a)
    go c = snd $ fromJust $ find ((==c) . fst) (map swap $ edgeListGraphToGrNodes li)

mapTheEdges :: Eq a => [(a, a)] -> [a] -> [LEdge String]
mapTheEdges [] li = []
mapTheEdges (x:xs) li =
  (mapTheTuples x li) : mapTheEdges xs li

edgeListGraphToGr :: (Eq a) => EdgeListGraph a -> Gr a String
edgeListGraphToGr (ELG x y) = 
  mkGraph (edgeListGraphToGrNodes x) (mapTheEdges y x)

edgeNextsToEdgeList :: (a -> [a]) -> [a] -> [(a, a)]
edgeNextsToEdgeList _ [] = []
edgeNextsToEdgeList f (x:xs) =
  [(x, y) | y <- f x] ++ edgeNextsToEdgeList f xs

nextFunGraphToEdgeListGraph :: NextFunGraph a -> EdgeListGraph a
nextFunGraphToEdgeListGraph (NFG x f) =
  ELG x (edgeNextsToEdgeList f x)

smallGraph :: Gr String String
smallGraph =
  edgeListGraphToGr (nextFunGraphToEdgeListGraph
                      (NFG ["A", "B", "C", "G"] smallGrNexts))

main :: IO FilePath
main = runGraphviz
  (graphToDot quickParams smallGraph) Pdf "graph.pdf"
