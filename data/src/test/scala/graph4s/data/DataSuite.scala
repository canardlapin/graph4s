package graph4s.data

import cats.Hash
import cats.Order
import graph4s.{Arc, Digraph, Edge, Graph}
import graph4s.syntax.*
import graph4s.data.syntax.*
import graph4s.indexed.VertexOrder
import graph4s.indexed.syntax.*
import munit.FunSuite
import scala.compiletime.testing.typeCheckErrors

final class DataSuite extends FunSuite:
  given Hash[String] = Hash.fromUniversalHashCode
  given Order[String] = Order.fromComparable

  test("edge fields have exactly one value per topology edge"):
    val graph = Graph.fromEdges("a" -- "b", "b" -- "c").toOption
    val field = graph.map(EdgeField.total(_)(_ => 2.5))
    assertEquals(field.map(_.size), Some(2L))
    assertEquals(field.flatMap(_.get("b", "a")), Some(2.5))

  test("induced subgraphs restrict vertex and edge fields"):
    val graph = Graph.fromEdges("a" -- "b", "b" -- "c").toOption
    val result = graph.map(_.inducedBy(_ != "c"))
    val vertexField = graph.map(VertexField.total(_)(_.length))
    val edgeField = graph.map(EdgeField.total(_)(_ => 1))
    assertEquals(
      (for
        induced <- result
        field <- vertexField
        restricted <- induced.restrict(field).toOption
      yield restricted.size),
      Some(2)
    )
    assertEquals(
      (for
        induced <- result
        field <- edgeField
        restricted <- induced.restrict(field).toOption
      yield restricted.size),
      Some(1L)
    )

  test("weighted graphs reject a mismatched topology"):
    val left = Graph.fromEdges("a" -- "b").toOption
    val right = Graph.fromEdges("b" -- "c").toOption
    val result =
      for
        topology <- left
        other <- right
        weights = EdgeField.total(other)(_ => 1.0)
      yield WeightedGraph(topology, weights)
    assert(result.exists(_.isLeft))

  test("partial maps accumulate unknown-domain errors"):
    val graph =
      Graph.fromEdges("a" -- "b").fold(errors => fail(errors.toString), identity)
    val vertices =
      VertexMap.from(graph, List("missing-1" -> 1, "missing-2" -> 2))
    val edges =
      EdgeMap.from(
        graph,
        List(
          Edge.unsafe("a", "missing") -> 1,
          Edge.unsafe("left", "right") -> 2
        )
      )
    assertEquals(vertices.swap.toOption.map(_.length), Some(2L))
    assertEquals(edges.swap.toOption.map(_.length), Some(2L))

  test("directed arc fields and weighted digraphs preserve arc direction"):
    val graph =
      Digraph
        .fromArcs("a" --> "b", "b" --> "a", "b" --> "c")
        .fold(errors => fail(errors.toString), identity)
    val field = ArcField.total(graph)(arc => s"${arc.source}->${arc.target}")
    val weighted = WeightedDigraph.from(field)

    assertEquals(field.size, 3L)
    assertEquals(field.get("a", "b"), Some("a->b"))
    assertEquals(field.get("b", "a"), Some("b->a"))
    assertEquals(weighted.topology.arcCount, 3L)

  test("indexed edge and arc fields follow their snapshot ordering"):
    val graph =
      Graph
        .fromEdges("a" -- "b", "b" -- "c")
        .fold(errors => fail(errors.toString), identity)
    val graphIndex =
      graph
        .indexed(VertexOrder.explicit(Vector("c", "b", "a")))
        .fold(errors => fail(errors.toString), identity)
    val edgeField =
      EdgeField.total(graph)(edge => Vector(edge.first, edge.second).sorted.mkString(""))
    val indexedEdges =
      IndexedEdgeField.from(graphIndex, edgeField).fold(error => fail(error.toString), identity)

    assertEquals(
      graphIndex.edges.map(indexedEdges.apply).toSet,
      Set("ab", "bc")
    )

    val digraph =
      Digraph
        .fromArcs("a" --> "b", "c" --> "b")
        .fold(errors => fail(errors.toString), identity)
    val digraphIndex =
      digraph
        .indexed(VertexOrder.explicit(Vector("c", "b", "a")))
        .fold(errors => fail(errors.toString), identity)
    val arcField = ArcField.total(digraph)(arc => s"${arc.source}->${arc.target}")
    val indexedArcs =
      IndexedArcField.from(digraphIndex, arcField).fold(error => fail(error.toString), identity)

    assertEquals(
      digraphIndex.arcs.map(indexedArcs.apply).toVector,
      Vector("c->b", "a->b")
    )

  test("partial arc maps accumulate unknown-domain errors"):
    val graph =
      Digraph
        .fromArcs("a" --> "b")
        .fold(errors => fail(errors.toString), identity)
    val arcs =
      ArcMap.from(
        graph,
        List(
          Arc.unsafe("missing", "b") -> 1,
          Arc.unsafe("a", "missing") -> 2
        )
      )
    assertEquals(arcs.swap.toOption.map(_.length), Some(2L))

  test("indexed fields reject coordinates from another snapshot at compile time"):
    val errors = typeCheckErrors(
      """
        import cats.{Hash, Order}
        import graph4s.*
        import graph4s.syntax.*
        import graph4s.data.*
        import graph4s.indexed.*
        import graph4s.indexed.syntax.*
        given Hash[String] = Hash.fromUniversalHashCode
        given Order[String] = Order.fromComparable
        val topology = Graph.fromEdges("a" -- "b").toOption.get
        val first = topology.indexed(VertexOrder.by(Order[String])).toOption.get
        val second = topology.indexed(VertexOrder.by(Order[String])).toOption.get
        val field = EdgeField.total(topology)(_ => 1.0)
        val aligned = IndexedEdgeField.from(first, field).toOption.get
        val foreign: second.Edge = second.edges.next()
        aligned(foreign)
      """
    )
    assert(errors.nonEmpty)
