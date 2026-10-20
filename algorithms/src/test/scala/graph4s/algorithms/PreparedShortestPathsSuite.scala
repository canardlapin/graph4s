package graph4s.algorithms

import cats.Hash
import graph4s.{Graph, Link}
import graph4s.indexed.{IndexedGraph, VertexOrder}

class PreparedShortestPathsSuite extends munit.FunSuite:
  private given Hash[Int] = Hash.fromUniversalHashCode
  private def ok[E, A](result: Either[E, A]): A = result.fold(e => fail(s"$e"), identity)
  private def indexed(vertices: Vector[Int], edges: Vector[(Int, Int)]): IndexedGraph[Int] =
    val graph = Graph.of(vertices, edges.map((a, b) => Link(a, b))).toOption.get
    IndexedGraph.from(graph, VertexOrder.explicit(vertices)).toOption.get

  test(
    "prepared distances agree with independent Floyd-Warshall across cyclic zero-cost and disconnected graphs"
  ):
    val vertices = (0 until 6).toVector
    val candidates = (for i <- vertices; j <- vertices if i < j yield (i, j)).toVector
    (0 until 48).foreach: seed =>
      val edges = candidates.zipWithIndex.collect:
        case (edge, k) if (seed * 17 + k * 7) % 11 < 5 => edge
      val weights =
        edges.zipWithIndex.map((edge, k) => edge -> ((seed + k * 3) % 7).toDouble / 4).toMap
      val graph = indexed(vertices, edges)
      val prepared = ok(PreparedShortestPaths.create(graph): edge =>
        val (a, b) = graph.endpoints(edge)
        weights((graph.label(a), graph.label(b))))
      val expected = Array.fill(6, 6)(Double.PositiveInfinity)
      vertices.foreach(i => expected(i)(i) = 0)
      weights.foreach: (pair, cost) =>
        expected(pair._1)(pair._2) = cost
        expected(pair._2)(pair._1) = cost
      for k <- vertices; i <- vertices; j <- vertices do
        expected(i)(j) = math.min(expected(i)(j), expected(i)(k) + expected(k)(j))
      vertices.foreach: source =>
        val result = prepared.distancesFrom(graph.vertex(source).get)
        vertices.foreach: target =>
          val actual = result.distanceTo(graph.vertex(target).get)
          if expected(source)(target).isPosInfinity then assertEquals(actual, None)
          else
            assertEqualsDouble(
              actual.get,
              expected(source)(target),
              0.0,
              s"seed=$seed $source->$target"
            )

  test("costs are evaluated once, snapshots and earlier source results remain immutable"):
    val graph = indexed(Vector(2, 0, 1, 3), Vector((0, 1), (1, 2)))
    var calls = 0
    var cost = 2.0
    val prepared = ok(PreparedShortestPaths.create(graph): _ =>
      calls += 1
      cost)
    assertEquals(calls, 2)
    val first = prepared.distancesFrom(graph.vertex(0).get)
    cost = 99
    val second = prepared.distancesFrom(graph.vertex(2).get)
    assertEquals(calls, 2)
    assertEqualsDouble(first.distanceTo(graph.vertex(2).get).get, 4, 0)
    assertEqualsDouble(second.distanceTo(graph.vertex(0).get).get, 4, 0)
    assertEquals(first.distanceTo(graph.vertex(3).get), None)
    assertEqualsDouble(first.distanceTo(graph.vertex(0).get).get, 0, 0)

  test("negative and nonfinite edge costs are typed refusals; all-zero and empty graphs are valid"):
    val graph = indexed(Vector(0, 1), Vector((0, 1)))
    Vector(-1.0, Double.NaN, Double.PositiveInfinity, Double.NegativeInfinity).foreach: value =>
      assert(PreparedShortestPaths.create(graph)(_ => value).isLeft)
    val zero = ok(PreparedShortestPaths.create(graph)(_ => 0.0))
    assertEquals(zero.distancesFrom(graph.vertex(0).get).distanceTo(graph.vertex(1).get), Some(0.0))
    val empty = indexed(Vector.empty, Vector.empty)
    assert(PreparedShortestPaths.create(empty)(_ => 0).isRight)

  test("overflow remains reachable and differs from no path; finite alternatives win"):
    val graph = indexed(Vector(0, 1, 2, 3, 4), Vector((0, 1), (1, 2), (2, 3)))
    val prepared = ok(PreparedShortestPaths.create(graph): edge =>
      if graph.edgeOrdinal(edge) == 2 then 0.0 else Double.MaxValue)
    val result = prepared.distancesFrom(graph.vertex(0).get)
    assertEquals(result.distanceTo(graph.vertex(2).get), Some(Double.PositiveInfinity))
    assertEquals(result.distanceTo(graph.vertex(3).get), Some(Double.PositiveInfinity))
    assertEquals(result.distanceTo(graph.vertex(4).get), None)
    assert(result.isReachable(graph.vertex(2).get))
    assert(!result.isReachable(graph.vertex(4).get))
    assert(result.distanceOrInfinity(graph.vertex(2).get).isPosInfinity)
    assert(result.distanceOrInfinity(graph.vertex(4).get).isPosInfinity)
    val alternative = indexed(Vector(0, 1, 2), Vector((0, 1), (1, 2), (0, 2)))
    val better = ok(PreparedShortestPaths.create(alternative): edge =>
      val (a, b) = alternative.endpoints(edge)
      if alternative.label(a) == 0 && alternative.label(b) == 2 then 3.0 else Double.MaxValue)
    assertEqualsDouble(
      better.distancesFrom(alternative.vertex(0).get).distanceTo(alternative.vertex(2).get).get,
      3,
      0
    )

  test("source and result lookup refuse another indexed owner at compile time"):
    assert(compileErrors("""
      import graph4s.algorithms.PreparedShortestPaths
      import graph4s.indexed.IndexedGraph
      def mix[V](a: IndexedGraph[V], b: IndexedGraph[V]) =
        val prepared = PreparedShortestPaths.create(a)(_ => 1.0).toOption.get
        prepared.distancesFrom(b.vertices.next())
    """).nonEmpty)
    assert(compileErrors("""
      import graph4s.algorithms.PreparedShortestPaths
      import graph4s.indexed.IndexedGraph
      def mix[V](a: IndexedGraph[V], b: IndexedGraph[V]) =
        val prepared = PreparedShortestPaths.create(a)(_ => 1.0).toOption.get
        val result = prepared.distancesFrom(a.vertices.next())
        result.distanceTo(b.vertices.next())
    """).nonEmpty)
