{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Core.Graph
  ( Graph (Graph),
    GraphNode (GraphNode),
    foldDepthFirst,
  )
where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

data Graph nodeId nodeData = Graph (Map nodeId (GraphNode nodeId nodeData))

data GraphNode nodeId nodeData = GraphNode nodeData (Set nodeId)

foldDepthFirst :: (Ord nodeId) => a -> (a -> nodeData -> a) -> nodeId -> Graph nodeId nodeData -> a
foldDepthFirst baseValue accumulator startNodeId graph = foldDepthFirstHelper Set.empty [startNodeId] baseValue accumulator graph

foldDepthFirstHelper :: (Ord nodeId) => Set nodeId -> [nodeId] -> a -> (a -> nodeData -> a) -> Graph nodeId nodeData -> a
foldDepthFirstHelper _ [] currentValue _ _ = currentValue
foldDepthFirstHelper touchedNodes (currentNodeId : restNodeStack) currentValue accumulator (Graph nodeMap) =
  foldDepthFirstHelper updatedTouchedNodes updatedNodeStack updatedValue accumulator (Graph nodeMap)
  where
    Just (GraphNode currentNodeData adjacentNodes) = Map.lookup currentNodeId nodeMap
    newNodes = Set.filter (\nodeId -> not (Set.member nodeId touchedNodes)) adjacentNodes
    updatedTouchedNodes = Set.union touchedNodes newNodes
    updatedNodeStack = Set.toList newNodes <> restNodeStack
    updatedValue = accumulator currentValue currentNodeData
