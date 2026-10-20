package graph4s.algorithms

import cats.collections.HashMap
import graph4s.{MissingVertex, Digraph}
import graph4s.data.WeightedDigraph
import scala.collection.mutable

/** A parallel choice on an arc. Order controls visitation when costs tie; it is independent of vertex hashing. */
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
    index.endpoints(source, target).map: (start, end) =>
      if start == end then Some(Vector.empty)
      else
        given Ordering[(Double, Int)] = Ordering.by[(Double, Int), Double](_._1).reverse
        val queue = mutable.PriorityQueue.empty[(Double, Int)]
        val distance = Array.fill(index.size)(Double.PositiveInfinity)
        val previous = Array.fill(index.size)(-1)
        val chosen = mutable.HashMap.empty[Int, E]
        val visited = Array.fill(index.size)(false)
        distance(start) = 0.0
        queue.enqueue((0.0, start))
        var reached = false
        while queue.nonEmpty && !reached do
          val (cost, node) = queue.dequeue()
          if !visited(node) then
            visited(node) = true
            if node == end then reached = true
            else
              index.adjacency(node).foreach: (next, choice) =>
                if !visited(next) then
                  val candidate = cost + choice.cost
                  if candidate < distance(next) then
                    distance(next) = candidate
                    previous(next) = node
                    chosen(next) = choice.value
                    queue.enqueue((candidate, next))
        if !reached then None
        else
          val route = Vector.newBuilder[E]
          var current = end
          while current != start do
            route += chosen(current)
            current = previous(current)
          Some(route.result().reverse)

  /** Every simple path; parallel choices are distinct. Ranking and limits are caller policy. */
  def allSimplePaths[V, E](
      graph: WeightedDigraph[V, Vector[RouteChoice[E]]],
      source: V,
      target: V
  ): Either[MissingVertex[V], Vector[Vector[E]]] =
    val index = new RouteIndex(graph)
    index.endpoints(source, target).map: (start, end) =>
      if start == end then Vector(Vector.empty)
      else
        val found = Vector.newBuilder[Vector[E]]
        val visited = Array.fill(index.size)(false)
        visited(start) = true
        def walk(node: Int, trail: List[E]): Unit =
          index.adjacency(node).foreach: (next, choice) =>
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
      topology.successorsOf(vertex).iterator.flatMap(_.iterator).flatMap: next =>
        graph.weights.get(vertex, next).iterator.flatten.map(choice => (positions.get(next).get, choice))
      .toVector.sortBy(_._2.order)

  def endpoints(source: V, target: V): Either[MissingVertex[V], (Int, Int)] =
    for
      start <- positions.get(source).toRight(MissingVertex.Vertex(source))
      end <- positions.get(target).toRight(MissingVertex.Vertex(target))
    yield (start, end)
