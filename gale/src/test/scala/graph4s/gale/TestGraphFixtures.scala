package graph4s.gale

import cats.Hash
import graph4s.{Arc, ArcInput, Digraph, Edge, Graph, Link}
import graph4s.data.{ArcField, EdgeField, IndexedArcField, IndexedEdgeField}
import graph4s.indexed.{IndexedDigraph, IndexedGraph, VertexOrder}

private[gale] final class UndirectedFixture[E] private (
    val topology: Graph[String],
    val index: IndexedGraph[String]
)(
    val weights: IndexedEdgeField[String, E, index.type]
)

private[gale] object UndirectedFixture:
  def apply[E](
      vertices: Vector[String],
      edges: Vector[(String, String, E)],
      order: Vector[String]
  ): UndirectedFixture[E] =
    given Hash[String] = Hash.fromUniversalHashCode
    val topology =
      Graph
        .of(vertices, edges.map((left, right, _) => Link(left, right)))
        .fold(errors => throw new IllegalArgumentException(errors.toString), identity)
    val index =
      IndexedGraph
        .from(topology, VertexOrder.explicit(order))
        .fold(errors => throw new IllegalArgumentException(errors.toString), identity)
    val field =
      EdgeField.total(topology): edge =>
        edgeValue(edge, edges).getOrElse(
          throw new IllegalArgumentException(s"missing fixture weight for $edge")
        )
    val aligned =
      IndexedEdgeField
        .from(index, field)
        .fold(error => throw new IllegalArgumentException(error.toString), identity)
    new UndirectedFixture(topology, index)(aligned)

  private def edgeValue[E](
      edge: Edge[String],
      entries: Vector[(String, String, E)]
  ): Option[E] =
    entries.collectFirst:
      case (left, right, value)
          if (left == edge.first && right == edge.second) ||
            (left == edge.second && right == edge.first) =>
        value

private[gale] final class DirectedFixture[E] private (
    val topology: Digraph[String],
    val index: IndexedDigraph[String]
)(
    val weights: IndexedArcField[String, E, index.type]
)

private[gale] object DirectedFixture:
  def apply[E](
      vertices: Vector[String],
      arcs: Vector[(String, String, E)],
      order: Vector[String]
  ): DirectedFixture[E] =
    given Hash[String] = Hash.fromUniversalHashCode
    val topology =
      Digraph
        .of(vertices, arcs.map((source, target, _) => ArcInput(source, target)))
        .fold(errors => throw new IllegalArgumentException(errors.toString), identity)
    val index =
      IndexedDigraph
        .from(topology, VertexOrder.explicit(order))
        .fold(errors => throw new IllegalArgumentException(errors.toString), identity)
    val field =
      ArcField.total(topology): arc =>
        arcValue(arc, arcs).getOrElse(
          throw new IllegalArgumentException(s"missing fixture weight for $arc")
        )
    val aligned =
      IndexedArcField
        .from(index, field)
        .fold(error => throw new IllegalArgumentException(error.toString), identity)
    new DirectedFixture(topology, index)(aligned)

  private def arcValue[E](
      arc: Arc[String],
      entries: Vector[(String, String, E)]
  ): Option[E] =
    entries.collectFirst:
      case (source, target, value) if source == arc.source && target == arc.target =>
        value

private[gale] object TestGraphFixtures:
  val vertices: Vector[String] =
    Vector("a", "b", "c", "d")

  def undirected[E](
      edges: Vector[(String, String, E)],
      order: Vector[String] = vertices
  ): UndirectedFixture[E] =
    UndirectedFixture(vertices, edges, order)

  def directed[E](
      arcs: Vector[(String, String, E)],
      order: Vector[String] = vertices
  ): DirectedFixture[E] =
    DirectedFixture(vertices, arcs, order)
