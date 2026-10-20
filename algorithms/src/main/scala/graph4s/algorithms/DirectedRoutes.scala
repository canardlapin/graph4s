package graph4s.algorithms

import cats.collections.HashMap
import graph4s.{MissingVertex, Digraph}
import graph4s.data.WeightedDigraph

/** A parallel choice on an arc. Order controls visitation when costs tie; it is independent of
  * vertex hashing.
  */
final case class RouteChoice[+E](value: E, cost: Double, order: Int):
  require(cost.isFinite && cost >= 0.0, "route cost must be finite and non-negative")

/** Weighted directed routes, retaining parallel arc choices and caller-defined edge order. */
object DirectedRoutes:
  def shortest[V, E](
      graph: WeightedDigraph[V, Vector[RouteChoice[E]]],
      source: V,
      target: V
  ): Either[MissingVertex[V], Option[Vector[E]]] =
    val index = new RouteIndex(graph)
    index
      .endpoints(source, target)
      .map: (start, end) =>
        if start == end then Some(Vector.empty)
        else
          val result =
            index.costs.search(start, Some(end), trackPath = true, includeOverflow = false)
          if !result.reachable(end) then None
          else
            val route = Vector.newBuilder[E]
            var current = end
            while current != start do
              route += index.choice(result.chosenArc(current))
              current = result.previous(current)
            Some(route.result().reverse)

  /** Every simple path; parallel choices are distinct. Ranking and limits are caller policy. */
  def allSimplePaths[V, E](
      graph: WeightedDigraph[V, Vector[RouteChoice[E]]],
      source: V,
      target: V
  ): Either[MissingVertex[V], Vector[Vector[E]]] =
    val index = new RouteIndex(graph)
    index
      .endpoints(source, target)
      .map: (start, end) =>
        if start == end then Vector(Vector.empty)
        else
          val found = Vector.newBuilder[Vector[E]]
          val visited = Array.fill(index.size)(false)
          visited(start) = true
          def walk(node: Int, trail: List[E]): Unit =
            index
              .adjacency(node)
              .foreach: (next, choice) =>
                if next == end then found += (choice.value :: trail).reverse.toVector
                else if !visited(next) then
                  visited(next) = true
                  walk(next, choice.value :: trail)
                  visited(next) = false
          walk(start, Nil)
          found.result()

private final class RouteIndex[V, E](graph: WeightedDigraph[V, Vector[RouteChoice[E]]]):
  private val topology: Digraph[V] = graph.topology
  private given cats.Hash[V] = topology.vertexHash
  private val labels = topology.vertices.iterator.toVector
  private val positions = HashMap.fromIterableOnce(labels.iterator.zipWithIndex)
  val size: Int = labels.size
  val adjacency: Vector[Vector[(Int, RouteChoice[E])]] =
    labels.map: vertex =>
      topology
        .successorsOf(vertex)
        .iterator
        .flatMap(_.iterator)
        .flatMap: next =>
          graph.weights
            .get(vertex, next)
            .iterator
            .flatten
            .map(choice => (positions.get(next).get, choice))
        .toVector
        .sortBy(_._2.order)

  private lazy val choices = adjacency.flatten.map(_._2.value)
  lazy val costs: NonNegativeCsr =
    NonNegativeCsr.fromRows(adjacency.map(_.map((next, choice) => (next, choice.cost))))

  def choice(arc: Int): E = choices(arc)

  def endpoints(source: V, target: V): Either[MissingVertex[V], (Int, Int)] =
    for
      start <- positions.get(source).toRight(MissingVertex.Vertex(source))
      end <- positions.get(target).toRight(MissingVertex.Vertex(target))
    yield (start, end)
