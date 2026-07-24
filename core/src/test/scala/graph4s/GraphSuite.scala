package graph4s

import cats.Hash
import cats.syntax.all.*
import graph4s.syntax.*
import munit.FunSuite

final class GraphSuite extends FunSuite:
  given Hash[String] = Hash.fromUniversalHashCode

  test("authoritative construction accumulates independent errors"):
    val result = Graph.of(
      vertices = List("a", "b"),
      edges = List(
        "a" -- "a",
        "a" -- "missing",
        "left" -- "right"
      )
    )
    val errors = result.swap.toOption.map(_.toChain.toList)
    assertEquals(errors.map(_.size), Some(4))

  test("duplicate edges have set semantics"):
    val graph = Graph
      .fromEdges("a" -- "b", "b" -- "a", "a" -- "b")
      .toOption
    assertEquals(graph.map(_.edgeCount), Some(1L))
    assertEquals(graph.map(_.vertexCount), Some(2))

  test("isolated and absent vertices remain distinct"):
    val graph = Graph.of(List("isolated"), List.empty).toOption
    assertEquals(graph.flatMap(_.neighborsOf("isolated")).map(_.size), Some(0))
    assertEquals(graph.flatMap(_.neighborsOf("absent")), None)

  test("persistent updates preserve the source value"):
    val source = Graph.singleton("a")
    val updated = source.connect("a", "b").toOption
    assertEquals(source.vertexCount, 1)
    assert(!source.containsEdge("a", "b"))
    assertEquals(updated.map(_.edgeCount), Some(1L))

  test("removeVertex removes exactly its incident edges"):
    val graph = Graph
      .fromEdges("a" -- "b", "b" -- "c", "c" -- "a")
      .toOption
    val removed = graph.map(_.removeVertex("b"))
    assertEquals(removed.map(_.vertexCount), Some(2))
    assertEquals(removed.map(_.edgeCount), Some(1L))
    assertEquals(removed.map(_.containsEdge("a", "c")), Some(true))

  test("graph equality is extensional"):
    val left = Graph.fromEdges("a" -- "b", "b" -- "c").toOption
    val right = Graph.fromEdges("c" -- "b", "b" -- "a").toOption
    assert(left.exists(a => right.exists(a === _)))

  test("complement is involutive"):
    val graph = Graph
      .of(List("a", "b", "c", "d"), List("a" -- "b", "c" -- "d"))
      .toOption
    assert(graph.exists(value => value.complement.complement === value))

  test("line graph makes incident edges adjacent"):
    val graph = Graph.fromEdges("a" -- "b", "b" -- "c", "d" -- "e").toOption
    val line = graph.map(_.lineGraph)
    assertEquals(line.map(_.vertexCount), Some(3))
    assertEquals(line.map(_.edgeCount), Some(1L))

  test("edges are symmetric public values"):
    val graph = Graph.fromEdges("a" -- "b").toOption
    val firstRead = graph.toVector.flatMap(_.edges)
    val secondRead = graph.toVector.flatMap(_.edges)
    assertEquals(firstRead, secondRead)
    assertEquals(firstRead.distinct.size, 1)
    assert(
      firstRead.headOption.exists(edge =>
        edge == Edge.unsafe("b", "a") &&
          edge.hashCode() == Edge.unsafe("b", "a").hashCode() &&
          edge.toString.startsWith("Edge(")
      )
    )

  test("vertex sets expose lawful set combinators"):
    val left = VertexSet.from(List("a", "b"))
    val right = VertexSet.from(List("b", "c"))
    assertEquals(left.union(right).size, 3)
    assertEquals(left.intersect(right), VertexSet.from(List("b")))
    assertEquals(left.diff(right), VertexSet.from(List("a")))

  test("natural graph combinators preserve their typed domains"):
    given Hash[Int] = Hash.fromUniversalHashCode
    val left = Graph.fromEdges("a" -- "b").toOption
    val right = Graph.fromEdges(1 -- 2).toOption
    val disjoint = for l <- left; r <- right yield l.disjointUnion(r)
    val joined = for l <- left; r <- right yield l.join(r)
    val product = for l <- left; r <- right yield l.cartesianProduct(r)
    assertEquals(disjoint.map(_.edgeCount), Some(2L))
    assertEquals(joined.map(_.edgeCount), Some(6L))
    assertEquals(product.map(_.vertexCount), Some(4))

  test("spanning filters and bidirected conversion retain all vertices"):
    val graph = Graph
      .of(List("a", "b", "isolated"), List("a" -- "b"))
      .toOption
    assertEquals(graph.map(_.spanningBy(_ => false).vertexCount), Some(3))
    assertEquals(graph.map(_.bidirected.arcCount), Some(2L))
