package graph4s.algorithms

import graph4s.indexed.IndexedGraph
import scala.collection.mutable

enum ShortestPathBuildError derives CanEqual:
  case InvalidEdgeCost(edge: Int, value: Double)

  def message: String = this match
    case InvalidEdgeCost(edge, value) =>
      s"edge $edge cost must be finite and non-negative, got $value"

/** Distances retain the exact indexed vertex owner. None means unreachable; Some(+Infinity) means
  * reachable with no representable finite path cost.
  */
final class SingleSourceDistances[V, G <: IndexedGraph[V]] private[algorithms] (
    val graph: G,
    private val costs: Array[Double],
    private val reachable: Array[Boolean]
):
  def distanceTo(vertex: graph.Vertex): Option[Double] =
    val i = graph.ordinal(vertex)
    Option.when(reachable(i))(costs(i))

  def isReachable(vertex: graph.Vertex): Boolean = reachable(graph.ordinal(vertex))

  /** Primitive read for callers whose policy represents both no path and overflow as +Infinity. */
  def distanceOrInfinity(vertex: graph.Vertex): Double = costs(graph.ordinal(vertex))

/** An immutable nonnegative cost snapshot, prepared once for many source queries. Each query owns
  * its scratch arrays; previous results remain immutable.
  */
final class PreparedShortestPaths[V, G <: IndexedGraph[V]] private[algorithms] (
    val graph: G,
    private val csr: NonNegativeCsr
):
  def distancesFrom(source: graph.Vertex): SingleSourceDistances[V, graph.type] =
    val result = csr.search(graph.ordinal(source), None, trackPath = false, includeOverflow = true)
    new SingleSourceDistances(graph, result.costs, result.reachable)

object PreparedShortestPaths:
  /** Evaluates each edge cost once and copies it into private primitive storage. */
  def create[V](graph: IndexedGraph[V])(
      cost: graph.Edge => Double
  ): Either[ShortestPathBuildError, PreparedShortestPaths[V, graph.type]] =
    val weights = new Array[Double](graph.edgeCount)
    var error: Option[ShortestPathBuildError] = None
    graph.edges.foreach: edge =>
      if error.isEmpty then
        val value = cost(edge)
        val ordinal = graph.edgeOrdinal(edge)
        if !value.isFinite || value < 0.0 then
          error = Some(ShortestPathBuildError.InvalidEdgeCost(ordinal, value))
        else weights(ordinal) = value
    error match
      case Some(value) => Left(value)
      case None        =>
        val offsets = new Array[Int](graph.vertexCount + 1)
        graph.vertices.foreach: vertex =>
          val i = graph.ordinal(vertex)
          offsets(i + 1) = offsets(i) + graph.degree(vertex)
        val targets = new Array[Int](offsets.last)
        val costs = new Array[Double](targets.length)
        val cursors = offsets.clone()
        graph.edges.foreach: edge =>
          val (a, b) = graph.endpoints(edge)
          val first = graph.ordinal(a)
          val second = graph.ordinal(b)
          val weight = weights(graph.edgeOrdinal(edge))
          targets(cursors(first)) = second
          costs(cursors(first)) = weight
          cursors(first) += 1
          targets(cursors(second)) = first
          costs(cursors(second)) = weight
          cursors(second) += 1
        Right(new PreparedShortestPaths(graph, new NonNegativeCsr(offsets, targets, costs)))

private[algorithms] final class DistanceSearch(
    val costs: Array[Double],
    val reachable: Array[Boolean],
    val previous: Array[Int],
    val chosenArc: Array[Int]
)

/** Shared primitive engine. Only validated builders may supply its owned arrays. */
private[algorithms] final class NonNegativeCsr(
    offsets: Array[Int],
    targets: Array[Int],
    costs: Array[Double]
):
  def search(
      source: Int,
      target: Option[Int],
      trackPath: Boolean,
      includeOverflow: Boolean
  ): DistanceSearch =
    val size = offsets.length - 1
    val distance = Array.fill(size)(Double.PositiveInfinity)
    val reachable = Array.fill(size)(false)
    val settled = Array.fill(size)(false)
    val previous = if trackPath then Array.fill(size)(-1) else Array.emptyIntArray
    val chosen = if trackPath then Array.fill(size)(-1) else Array.emptyIntArray
    given Ordering[(Double, Int)] = Ordering.by[(Double, Int), Double](_._1).reverse
    val queue = mutable.PriorityQueue.empty[(Double, Int)]
    distance(source) = 0.0
    reachable(source) = true
    queue.enqueue((0.0, source))
    var finished = false
    while queue.nonEmpty && !finished do
      val (cost, vertex) = queue.dequeue()
      if !settled(vertex) then
        settled(vertex) = true
        if target.contains(vertex) then finished = true
        else
          var arc = offsets(vertex)
          while arc < offsets(vertex + 1) do
            val next = targets(arc)
            if !settled(next) then
              val candidate = cost + costs(arc)
              if candidate < distance(next) || (includeOverflow && !reachable(next)) then
                distance(next) = candidate
                reachable(next) = true
                if trackPath then
                  previous(next) = vertex
                  chosen(next) = arc
                queue.enqueue((candidate, next))
            arc += 1
    new DistanceSearch(distance, reachable, previous, chosen)

private[algorithms] object NonNegativeCsr:
  def fromRows(rows: Vector[Vector[(Int, Double)]]): NonNegativeCsr =
    val offsets = new Array[Int](rows.length + 1)
    rows.indices.foreach(i => offsets(i + 1) = offsets(i) + rows(i).length)
    val targets = new Array[Int](offsets.last)
    val costs = new Array[Double](targets.length)
    rows.indices.foreach: vertex =>
      var arc = offsets(vertex)
      rows(vertex).foreach: (next, cost) =>
        targets(arc) = next
        costs(arc) = cost
        arc += 1
    new NonNegativeCsr(offsets, targets, costs)
