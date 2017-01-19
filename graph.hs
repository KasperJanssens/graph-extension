#!/usr/bin/env stack
-- stack runghc --resolver lts-7.14 --install-ghc

import qualified Data.Graph as Graph

type Node = String

type Key = Int

type Place a = (a, Key)

type Route = [Graph.Vertex]

nodesInReach'::(Graph.Vertex -> [Graph.Vertex]) -> Int -> Route -> [Route]
nodesInReach' _ 0 curRoute = [curRoute]
nodesInReach' lookupNext reach curRoute =
  let nextVertices = lookupNext $ last curRoute in
  let newRoutes = fmap (\vertex -> curRoute ++ [vertex]) nextVertices in
  concatMap (nodesInReach' lookupNext (reach - 1)) newRoutes
    
nextVertex::(Graph.Vertex -> (a, Key, [Key])) -> (Key -> Maybe Graph.Vertex) -> (Graph.Vertex -> [Graph.Vertex] -> [Graph.Vertex]) -> Graph.Vertex -> [Graph.Vertex]
nextVertex lookupVertex lookupKey vertexFilter currentVertex =
  let (_, _, nextKeys) = lookupVertex currentVertex in
  maybe [] (vertexFilter currentVertex) $ mapM lookupKey nextKeys

noGoingBack = \v -> filter (v /= )

goingBack = \_ vs -> vs

data BoardConfig = GoingBack | NoGoingBack

getFilter:: BoardConfig ->(Graph.Vertex -> [Graph.Vertex] -> [Graph.Vertex]) 
getFilter GoingBack =  goingBack
getFilter NoGoingBack =  noGoingBack

nodesInReach::BoardConfig -> (Graph.Vertex -> (a, Key, [Key])) -> (Key -> Maybe Graph.Vertex) -> Int -> Place String -> Maybe [Route]
nodesInReach boardConfig lookupVertex lookupKey reach (_, key)=
  let maybeVertex = lookupKey key in
  let filter = getFilter boardConfig in
  fmap (\v -> nodesInReach' (nextVertex lookupVertex lookupKey filter) reach [v]) maybeVertex

rooms:: [(Node, Key, [Key])]
rooms = [
  ("Living", 0, [1,2]),
  ("Keuken", 1, [2,0]),
  ("Hall", 2, [3,4,0,1]),
  ("Berging", 3, [2]),
  ("Lage Trap", 4,[5,6,7,2]),
  ("Bibliotheek", 5, [4]),
  ("Kleine Slaapkamer", 6, [4]),
  ("Hoge Trap", 7,[8,9,4]),
  ("Badkamer", 8, [7]),
  ("Slaapkamer", 9, [7])
 ]

main:: IO ()
main = do
  let (graph, lookupNodeFromVertex, lookupVertexFromKey) = Graph.graphFromEdges rooms
  print "Living to badkamer"
  print $ Graph.path graph 0 8
  print "Living to keuken"
  print $ Graph.path graph 0 1
  print "Keuken to hall"
  print $ Graph.path graph 1 2
  print "hall to lage trap"
  print $ Graph.path graph 2 4
  print "living to lage trap"
  print $ Graph.path graph 0 4
  print "badkamer to living"
  print $ Graph.path graph 8 0
  print "vertices"
  print $ Graph.vertices graph
  print $ lookupNodeFromVertex 0
  print $ nodesInReach NoGoingBack lookupNodeFromVertex lookupVertexFromKey 4 ("", 0)



