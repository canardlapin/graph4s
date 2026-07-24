package graph4s.expr

import cats.Hash
import cats.Functor
import cats.syntax.all.*
import munit.FunSuite

final class ExpressionSuite extends FunSuite:
  given Hash[String] = Hash.fromUniversalHashCode

  test("star compiles without edges between leaves"):
    val graph = GraphExpr.star("hub", List("a", "b", "c")).compile
    assertEquals(graph.vertexCount, 4)
    assertEquals(graph.edgeCount, 3L)
    assert(!graph.containsEdge("a", "b"))

  test("overlay laws hold extensionally"):
    val x = GraphExpr.path(List("a", "b", "c"))
    val y = GraphExpr.vertex("d")
    assert(x.overlay(GraphExpr.Empty).compile === x.compile)
    assert(x.overlay(x).compile === x.compile)
    assert(x.overlay(y).compile === y.overlay(x).compile)

  test("deep overlays compile without recursive interpreter growth"):
    val expression =
      (1 to 20000).foldLeft(GraphExpr.empty[Int])((acc, value) =>
        acc.overlay(GraphExpr.vertex(value))
      )
    given Hash[Int] = Hash.fromUniversalHashCode
    assertEquals(expression.compile.vertexCount, 20000)

  test("directed connect has algebraic cross-product semantics"):
    val graph =
      DigraphExpr
        .vertex("read")
        .connect(
          DigraphExpr
            .vertex("parse")
            .overlay(DigraphExpr.vertex("lint"))
        )
        .connect(DigraphExpr.vertex("emit"))
        .compile
    assert(graph.containsArc("read", "emit"))
    assert(graph.containsArc("parse", "emit"))
    assert(graph.containsArc("lint", "emit"))

  test("complete digraph has both orientations and no loops"):
    val graph = DigraphExpr.complete(List("a", "b", "c")).compile
    assertEquals(graph.arcCount, 6L)
    assert(!graph.containsArc("a", "a"))

  test("named undirected constructors have simple-graph semantics"):
    val cycle = GraphExpr.cycle(List("a", "b", "c", "d")).compile
    val clique = GraphExpr.clique(List("a", "b", "c", "d")).compile
    val biclique = GraphExpr.biclique(List("a", "b"), List("c", "d")).compile
    assertEquals(cycle.edgeCount, 4L)
    assertEquals(clique.edgeCount, 6L)
    assertEquals(biclique.edgeCount, 4L)

  test("deep functor mapping uses the stack-safe zipper"):
    given Hash[Int] = Hash.fromUniversalHashCode
    val expression =
      (1 to 20000).foldLeft(GraphExpr.empty[Int])((acc, value) =>
        acc.overlay(GraphExpr.vertex(value))
      )
    val mapped = Functor[GraphExpr].map(expression)(_ + 1)
    assertEquals(mapped.compile.vertexCount, 20000)
