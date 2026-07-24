package graph4s.indexed

import cats.{Hash, Order}
import cats.syntax.all.*
import graph4s.{Digraph, Graph}
import graph4s.syntax.*
import graph4s.indexed.syntax.*
import munit.FunSuite
import scala.compiletime.testing.typeCheckErrors

final class IndexedSuite extends FunSuite:
  given Hash[String] = Hash.fromUniversalHashCode
  given Order[String] = Order.fromComparable

  test("undirected CSR round-trips extensionally"):
    val expected =
      Graph
        .of(
          List("a", "b", "c", "isolated"),
          List("a" -- "b", "b" -- "c")
        )
        .fold(errors => fail(errors.toString), identity)
    val indexed =
      expected
        .indexed(VertexOrder.by(Order[String]))
        .fold(errors => fail(errors.toString), identity)
    assert(indexed.toGraph === expected)

  test("explicit ordering accumulates permutation errors"):
    val result =
      Graph
        .fromEdges("a" -- "b", "b" -- "c")
        .andThen(_.indexed(VertexOrder.explicit(Vector("a", "a", "foreign"))))
    assertEquals(result.swap.toOption.map(_.length), Some(4L))

  test("directed CSR and CSC agree with the source"):
    val expected =
      Digraph
        .fromArcs("a" --> "b", "c" --> "b")
        .fold(errors => fail(errors.toString), identity)
    val indexed =
      expected
        .indexed(VertexOrder.by(Order[String]))
        .fold(errors => fail(errors.toString), identity)
    assertEquals(indexed.arcCount, 2)
    assert(cats.Eq[Digraph[String]].eqv(indexed.toDigraph, expected))

  test("graph-scoped vertices provide explicit dense ordinals"):
    val graph =
      Graph
        .fromEdges("a" -- "b", "b" -- "c")
        .fold(errors => fail(errors.toString), identity)
    val indexed =
      graph
        .indexed(VertexOrder.by(Order[String]))
        .fold(errors => fail(errors.toString), identity)
    val degrees = Array.fill(indexed.vertexCount)(0)
    indexed.vertices.foreach { vertex =>
      degrees(indexed.ordinal(vertex)) = indexed.degree(vertex)
    }
    assertEquals(degrees.toVector, Vector(1, 2, 1))
    assertEquals(indexed.vertexAt(-1), None)
    assertEquals(indexed.vertexAt(indexed.vertexCount), None)

  test("array addressability failures retain the requested size"):
    val entries = Int.MaxValue.toLong + 1L
    assertEquals(
      IndexingLimits.arrayLength[String](entries),
      Left(IndexingError.AdjacencyTooLarge(entries))
    )

  test("vertices are graph-scoped at compile time"):
    val errors = typeCheckErrors(
      """
        import cats.{Hash, Order}
        import graph4s.*
        import graph4s.syntax.*
        import graph4s.indexed.*
        import graph4s.indexed.syntax.*
        given Hash[Int] = Hash.fromUniversalHashCode
        given Order[Int] = Order.fromOrdering
        val ga = Graph.fromEdges(1 -- 2).toOption.get
        val gb = Graph.fromEdges(3 -- 4).toOption.get
        val a = ga.indexed(VertexOrder.by(Order[Int])).toOption.get
        val b = gb.indexed(VertexOrder.by(Order[Int])).toOption.get
        val bv: b.Vertex = b.vertex(3).get
        a.neighbors(bv)
      """
    )
    assert(errors.nonEmpty)
