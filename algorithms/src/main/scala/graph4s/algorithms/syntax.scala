package graph4s.algorithms

import cats.Order
import graph4s.{ConnectivityError, Digraph, Graph, MissingVertex}

object syntax:
  extension [V](graph: Graph[V])
    def bfsFrom(root: V)(using Order[V]): Either[MissingVertex[V], BfsResult[V]] =
      GraphAlgorithms.bfs(graph, root)

    def dfsFrom(root: V)(using Order[V]): Either[MissingVertex[V], DfsResult[V]] =
      GraphAlgorithms.dfs(graph, root)

    def shortestPath(
        source: V,
        target: V
    )(using Order[V]): Either[MissingVertex[V], Option[Path[V]]] =
      GraphAlgorithms.shortestPath(graph, source, target)

    def connectedComponents(using Order[V]): Components[V] =
      GraphAlgorithms.connectedComponents(graph)

    def requireConnected(using
        Order[V]
    ): Either[ConnectivityError[V], ConnectedGraph[V]] =
      GraphAlgorithms.requireConnected(graph)

  extension [V](graph: Digraph[V])
    def bfsFrom(root: V)(using Order[V]): Either[MissingVertex[V], BfsResult[V]] =
      DigraphAlgorithms.bfs(graph, root)

    def stronglyConnectedComponents(using Order[V]): Components[V] =
      DigraphAlgorithms.stronglyConnectedComponents(graph)

    def weaklyConnectedComponents(using Order[V]): Components[V] =
      DigraphAlgorithms.weaklyConnectedComponents(graph)

    def topologicalSort(using
        Order[V]
    ): Either[DirectedCycle[V], Vector[V]] =
      DigraphAlgorithms.topologicalSort(graph)

    def requireDag(using Order[V]): Either[DirectedCycle[V], Dag[V]] =
      DigraphAlgorithms.requireDag(graph)
