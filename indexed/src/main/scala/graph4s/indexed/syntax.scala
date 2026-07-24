package graph4s.indexed

import cats.data.ValidatedNec
import graph4s.{Digraph, Graph}

object syntax:
  extension [V](graph: Graph[V])
    def indexed(
        order: VertexOrder[V]
    ): ValidatedNec[IndexingError[V], IndexedGraph[V]] =
      IndexedGraph.from(graph, order)

  extension [V](graph: Digraph[V])
    def indexed(
        order: VertexOrder[V]
    ): ValidatedNec[IndexingError[V], IndexedDigraph[V]] =
      IndexedDigraph.from(graph, order)
