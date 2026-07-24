package graph4s.algorithms

import cats.{Hash, Order}
import graph4s.{ConnectivityError, Digraph, Graph, MissingVertex}
import graph4s.syntax.*
import graph4s.algorithms.syntax.*
import munit.FunSuite

final class AlgorithmsSuite extends FunSuite:
  given Hash[String] = Hash.fromUniversalHashCode
  given Order[String] = Order.fromComparable

  test("BFS returns distances, parents, and paths"):
    val graph = Graph.fromEdges("a" -- "b", "b" -- "c", "a" -- "d").toOption
    val bfs = graph.flatMap(_.bfsFrom("a").toOption)
    assertEquals(bfs.flatMap(_.distance.get("c")), Some(2))
    assertEquals(bfs.flatMap(_.pathTo("c")).map(_.vertices.toVector), Some(Vector("a", "b", "c")))

  test("shortest path distinguishes missing endpoints from no path"):
    val graph = Graph
      .of(List("a", "b", "isolated"), List("a" -- "b"))
      .toOption
    assertEquals(
      graph.flatMap(_.shortestPath("a", "isolated").toOption).flatten,
      None
    )
    val missing = graph.map(_.shortestPath("left", "right"))
    assert(missing.exists {
      case Left(MissingVertex.Vertices(values)) => values.length == 2
      case _                                    => false
    })

  test("components are empty for the empty graph"):
    val graph = Graph.empty[String]
    assertEquals(graph.connectedComponents.components, Vector.empty)
    assertEquals(
      graph.requireConnected,
      Left(ConnectivityError.EmptyGraph[String]())
    )

  test("connected evidence retains a spanning tree"):
    val graph = Graph.fromEdges("a" -- "b", "b" -- "c").toOption
    val connected = graph.flatMap(_.requireConnected.toOption)
    assertEquals(connected.map(_.spanningTree.parent.size), Some(3))

  test("topological sort is deterministic and DAG evidence caches layers"):
    val graph = Digraph
      .fromArcs("read" --> "parse", "lint" --> "emit", "parse" --> "emit")
      .toOption
    val dag = graph.flatMap(_.requireDag.toOption)
    assertEquals(
      dag.map(_.topologicalOrder),
      Some(Vector("lint", "read", "parse", "emit"))
    )
    assert(dag.exists(_.layers.nonEmpty))

  test("directed cycles return an actual cycle witness"):
    val graph = Digraph
      .fromArcs("a" --> "b", "b" --> "c", "c" --> "a")
      .toOption
    val cycle = graph.flatMap(_.topologicalSort.swap.toOption)
    assertEquals(cycle.map(_.vertices.length), Some(3))

  test("strong components respect arc direction"):
    val graph = Digraph
      .fromArcs(
        "a" --> "b",
        "b" --> "a",
        "b" --> "c",
        "c" --> "d",
        "d" --> "c"
      )
      .toOption
    assertEquals(
      graph.map(_.stronglyConnectedComponents.components.map(_.size).sorted),
      Some(Vector(2, 2))
    )

  test("DFS parent map is the tree that produced its preorder"):
    given Hash[Int] = Hash.fromUniversalHashCode
    given Order[Int] = Order.fromOrdering
    val graph =
      Graph
        .fromEdges(
          0 -- 1,
          0 -- 2,
          1 -- 3,
          1 -- 4,
          3 -- 2
        )
        .fold(errors => fail(errors.toString), identity)
    val result =
      graph.dfsFrom(0).fold(error => fail(error.toString), identity)
    assertEquals(result.preorder, Vector(0, 1, 3, 2, 4))
    assertEquals(result.parent.get(1), Some(Some(0)))
    assertEquals(result.parent.get(2), Some(Some(3)))
    assertEquals(result.parent.get(3), Some(Some(1)))
    assertEquals(result.parent.get(4), Some(Some(1)))

  test("directed BFS follows successors only"):
    val graph =
      Digraph
        .fromArcs("a" --> "b", "c" --> "b", "b" --> "d")
        .fold(errors => fail(errors.toString), identity)
    val fromA = graph.bfsFrom("a").fold(error => fail(error.toString), identity)
    val fromB = graph.bfsFrom("b").fold(error => fail(error.toString), identity)
    assertEquals(fromA.distance.get("d"), Some(2))
    assertEquals(fromB.distance.get("a"), None)

  test("weak components ignore arc orientation"):
    val graph =
      Digraph
        .fromArcs("a" --> "b", "d" --> "c")
        .fold(errors => fail(errors.toString), identity)
        .addVertex("isolated")
    assertEquals(
      graph.weaklyConnectedComponents.components.map(_.size).sorted,
      Vector(1, 2, 2)
    )

  test("paths are nonempty values whose length counts edges"):
    val one = Path.one("a")
    val many =
      Path
        .fromVertices(Vector("a", "b", "c"))
        .fold(fail("nonempty path was rejected"))(identity)
    assertEquals(one.length, 0)
    assertEquals(many.length, 2)
    assertEquals(Path.fromVertices(Vector("a", "b", "c")), Some(many))

  test("structured algorithm results have repeatable value equality"):
    val graph =
      Graph
        .fromEdges("a" -- "b", "b" -- "c")
        .fold(errors => fail(errors.toString), identity)
    assertEquals(graph.bfsFrom("a"), graph.bfsFrom("a"))
    assertEquals(graph.dfsFrom("a"), graph.dfsFrom("a"))
    assertEquals(graph.connectedComponents, graph.connectedComponents)
