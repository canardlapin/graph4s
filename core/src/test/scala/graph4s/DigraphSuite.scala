package graph4s

import cats.Hash
import graph4s.syntax.*
import munit.FunSuite

final class DigraphSuite extends FunSuite:
  given Hash[String] = Hash.fromUniversalHashCode

  test("successor and predecessor indexes agree"):
    val graph = Digraph
      .fromArcs("a" --> "b", "a" --> "c", "c" --> "b")
      .toOption
    assertEquals(graph.flatMap(_.outDegreeOf("a")), Some(2))
    assertEquals(graph.flatMap(_.inDegreeOf("b")), Some(2))
    assertEquals(graph.map(_.arcCount), Some(3L))

  test("transpose is involutive"):
    val graph = Digraph.fromArcs("a" --> "b", "c" --> "b").toOption
    assert(graph.exists(value => cats.Eq[Digraph[String]].eqv(value.transpose.transpose, value)))

  test("removing a vertex removes incoming and outgoing arcs"):
    val graph = Digraph
      .fromArcs("a" --> "b", "b" --> "c", "c" --> "b")
      .toOption
    val removed = graph.map(_.removeVertex("b"))
    assertEquals(removed.map(_.arcCount), Some(0L))
    assertEquals(removed.map(_.vertexCount), Some(2))

  test("arcs are directional public values"):
    val graph = Digraph.fromArcs("a" --> "b").toOption
    val firstRead = graph.toVector.flatMap(_.arcs)
    val secondRead = graph.toVector.flatMap(_.arcs)
    assertEquals(firstRead, secondRead)
    assertEquals(firstRead.distinct.size, 1)
    assert(
      firstRead.headOption.exists(arc =>
        arc != Arc.unsafe("b", "a") &&
          arc == Arc.unsafe("a", "b") &&
          arc.toString == "Arc(a, b)"
      )
    )
