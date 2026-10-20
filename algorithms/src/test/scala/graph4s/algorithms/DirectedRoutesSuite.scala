package graph4s.algorithms

import cats.Hash
import graph4s.{Digraph, MissingVertex}
import graph4s.data.{ArcField, WeightedDigraph}
import graph4s.syntax.*

class DirectedRoutesSuite extends munit.FunSuite:
  private case class Edge(id: String, from: String, to: String, cost: Double)
  private given Hash[String] = Hash.fromUniversalHashCode

  private def graph(edges: Vector[Edge]) =
    val topology = Digraph.of(Vector("a", "b", "c", "isolated"), edges.map(e => e.from --> e.to)).toOption.get
    WeightedDigraph.from(ArcField.total(topology): arc =>
      edges.zipWithIndex.collect:
        case (edge, index) if edge.from == arc.source && edge.to == arc.target => RouteChoice(edge.id, edge.cost, index)
    )

  test("parallel choices retain identity and least total cost wins"):
    val g = graph(Vector(Edge("expensive", "a", "b", 4.0), Edge("cheap", "a", "b", 1.0), Edge("bc", "b", "c", 1.0), Edge("direct", "a", "c", 3.0)))
    assertEquals(DirectedRoutes.shortest(g, "a", "c"), Right(Some(Vector("cheap", "bc"))))
    assertEquals(DirectedRoutes.shortest(g, "a", "isolated"), Right(None))
    assertEquals(DirectedRoutes.shortest(g, "a", "missing"), Left(MissingVertex.Vertex("missing")))

  test("all simple paths retain parallel choices and terminate on cycles"):
    val g = graph(Vector(Edge("ab1", "a", "b", 1.0), Edge("ab2", "a", "b", 1.0), Edge("ba", "b", "a", 0.0), Edge("bc", "b", "c", 1.0), Edge("ac", "a", "c", 3.0)))
    assertEquals(DirectedRoutes.allSimplePaths(g, "a", "c"), Right(Vector(Vector("ab1", "bc"), Vector("ab2", "bc"), Vector("ac"))))
    assertEquals(DirectedRoutes.shortest(g, "a", "c"), Right(Some(Vector("ab1", "bc"))))
    assertEquals(DirectedRoutes.allSimplePaths(g, "a", "a"), Right(Vector(Vector.empty)))

  test("Dijkstra cost agrees with independent exhaustive DAG enumeration"):
    val edges = Vector(Edge("ab", "a", "b", 0.0), Edge("ab2", "a", "b", 1.0), Edge("bc", "b", "c", 0.25), Edge("ac", "a", "c", 0.5))
    val costs = Vector(0.0 + 0.25, 1.0 + 0.25, 0.5)
    val path = DirectedRoutes.shortest(graph(edges), "a", "c").toOption.flatten.get
    assertEqualsDouble(path.map(id => edges.find(_.id == id).get.cost).sum, costs.min, 0.0)

  test("route choices reject non-finite and negative costs"):
    Vector(Double.NaN, Double.PositiveInfinity, -1.0).foreach: cost =>
      intercept[IllegalArgumentException](RouteChoice("bad", cost, 0))

  test("routes respect the topology's custom Hash rather than universal equality"):
    final case class Node(label: String, annotation: Int)
    given Hash[Node] with
      def hash(node: Node): Int = node.label.hashCode
      def eqv(left: Node, right: Node): Boolean = left.label == right.label
    val a = Node("a", 1)
    val b = Node("b", 2)
    val c = Node("c", 3)
    val topology = Digraph.of(Vector(a, b, c), Vector(a --> b, b --> c)).toOption.get
    val g = WeightedDigraph.from(ArcField.total(topology)(arc => Vector(RouteChoice(arc.target.label, 1.0, 0))))
    assertEquals(DirectedRoutes.shortest(g, Node("a", 99), Node("c", 88)), Right(Some(Vector("b", "c"))))
    assertEquals(DirectedRoutes.allSimplePaths(g, Node("a", 99), Node("c", 88)), Right(Vector(Vector("b", "c"))))

  test("shortest costs agree with an independent Floyd-Warshall oracle across cyclic graphs"):
    val vertices = Vector("a", "b", "c", "isolated")
    for seed <- 0 until 48 do
      val candidates = for i <- vertices.indices; j <- vertices.indices if i != j yield (i, j)
      val edges = candidates.zipWithIndex.collect:
        case ((i, j), k) if ((seed * 17 + k * 7) % 11) < 5 => Edge(s"$i-$j", vertices(i), vertices(j), ((seed + k * 3) % 7).toDouble / 4.0)
      val distances = Array.fill(4, 4)(Double.PositiveInfinity)
      vertices.indices.foreach(i => distances(i)(i) = 0.0)
      edges.foreach(e => distances(vertices.indexOf(e.from))(vertices.indexOf(e.to)) = e.cost)
      for k <- vertices.indices; i <- vertices.indices; j <- vertices.indices do
        distances(i)(j) = math.min(distances(i)(j), distances(i)(k) + distances(k)(j))
      val topology = Digraph.of(vertices, edges.map(e => e.from --> e.to)).toOption.get
      val g = WeightedDigraph.from(ArcField.total(topology): arc =>
        edges.zipWithIndex.collect:
          case (e, k) if e.from == arc.source && e.to == arc.target => RouteChoice(e.id, e.cost, k)
        .toVector
      )
      for i <- vertices.indices; j <- vertices.indices do
        val route = DirectedRoutes.shortest(g, vertices(i), vertices(j)).toOption.get
        if distances(i)(j).isPosInfinity then assertEquals(route, None)
        else
          val cost = route.get.map(id => edges.find(_.id == id).get.cost).sum
          assertEqualsDouble(cost, distances(i)(j), 0.0, s"seed=$seed from=$i to=$j")
