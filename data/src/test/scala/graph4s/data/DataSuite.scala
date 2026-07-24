package graph4s.data

import cats.Hash
import graph4s.{Edge, Graph}
import graph4s.syntax.*
import graph4s.data.syntax.*
import munit.FunSuite

final class DataSuite extends FunSuite:
  given Hash[String] = Hash.fromUniversalHashCode

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
