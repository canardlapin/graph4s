package graph4s.algorithms

import cats.Order
import cats.data.NonEmptyVector
import graph4s.data.VertexMap
import graph4s.indexed.{IndexedDigraph, VertexOrder}
import graph4s.{Digraph, MissingVertex, VertexSet}
import scala.collection.mutable

object DigraphAlgorithms:
  def bfs[V](
      graph: Digraph[V],
      root: V
  )(using order: Order[V]): Either[MissingVertex[V], BfsResult[V]] =
    if !graph.containsVertex(root) then Left(MissingVertex.Vertex(root))
    else
      val index = DenseDirectedIndex(graph, order)
      index.position(root) match
        case None        => Left(MissingVertex.Vertex(root))
        case Some(start) => Right(DenseTraversal.bfs(graph, index, start))

  def stronglyConnectedComponents[V](
      graph: Digraph[V]
  )(using order: Order[V]): Components[V] =
    val index = DenseDirectedIndex(graph, order)
    val finish = finishOrder(index)
    val assigned = Array.fill(index.size)(false)
    val componentSets = Vector.newBuilder[VertexSet[V]]
    val membership = Vector.newBuilder[(V, Int)]
    var componentIndex = 0

    finish.reverseIterator.foreach { start =>
      if !assigned(start) then
        val stack = mutable.ArrayDeque(start)
        val labels = Vector.newBuilder[V]
        assigned(start) = true
        while stack.nonEmpty do
          val current = stack.removeLast()
          val label = index.label(current)
          labels += label
          membership += ((label, componentIndex))
          index.reversePredecessors(current).foreach { next =>
            if !assigned(next) then
              assigned(next) = true
              stack.append(next)
          }
        componentSets += VertexSet.from(labels.result())(using graph.vertexHash)
        componentIndex += 1
    }

    Components(
      componentSets.result(),
      VertexMap.fromTrusted(graph, membership.result())
    )

  def weaklyConnectedComponents[V](
      graph: Digraph[V]
  )(using Order[V]): Components[V] =
    GraphAlgorithms.connectedComponents(graph.underlyingGraph)

  def topologicalSort[V](
      graph: Digraph[V]
  )(using order: Order[V]): Either[DirectedCycle[V], Vector[V]] =
    topologicalData(graph).map(_._1)

  def requireDag[V](
      graph: Digraph[V]
  )(using order: Order[V]): Either[DirectedCycle[V], Dag[V]] =
    topologicalData(graph).map { case (topologicalOrder, layers) =>
      new Dag(graph, topologicalOrder, layers)
    }

  private def topologicalData[V](
      graph: Digraph[V]
  )(using order: Order[V]): Either[DirectedCycle[V], (Vector[V], Vector[NonEmptyVector[V]])] =
    val index = DenseDirectedIndex(graph, order)
    val inDegree = Array.tabulate(index.size)(index.inDegree)
    var frontier =
      inDegree.indices.iterator.filter(inDegree(_) == 0).toVector
    val result = Vector.newBuilder[V]
    val layers = Vector.newBuilder[NonEmptyVector[V]]
    var processed = 0

    while frontier.nonEmpty do
      val layerLabels = frontier.map(index.label)
      NonEmptyVector.fromVector(layerLabels).foreach(layers += _)
      val next = mutable.ArrayBuffer.empty[Int]
      frontier.foreach { current =>
        result += index.label(current)
        processed += 1
        index.successors(current).foreach { target =>
          inDegree(target) -= 1
          if inDegree(target) == 0 then next += target
        }
      }
      frontier = next.toVector.sorted

    if processed == index.size then Right((result.result(), layers.result()))
    else Left(cycleWitness(index, inDegree))

  /** Kahn's residual has no zero-indegree vertex. Following residual predecessors must therefore
    * repeat and yields a directed cycle.
    */
  private def cycleWitness[V](
      index: DenseDirectedIndex[V],
      residualInDegree: Array[Int]
  ): DirectedCycle[V] =
    val residual = residualInDegree.map(_ > 0)
    var current = residual.indexWhere(identity)
    val seenAt = Array.fill(index.size)(-1)
    val walk = mutable.ArrayBuffer.empty[Int]

    while seenAt(current) < 0 do
      seenAt(current) = walk.size
      walk += current
      var predecessor = -1
      val candidates = index.predecessors(current)
      while candidates.hasNext && predecessor < 0 do
        val candidate = candidates.next()
        if residual(candidate) then predecessor = candidate
      current = predecessor

    val first = seenAt(current)
    val backward = walk.slice(first, walk.size)
    val headOrdinal = backward(backward.size - 1)
    val tail =
      (backward.size - 2 to 0 by -1).iterator
        .map(position => index.label(backward(position)))
        .toVector
    DirectedCycle(NonEmptyVector(index.label(headOrdinal), tail))

  private def finishOrder[V](
      index: DenseDirectedIndex[V]
  ): Vector[Int] =
    val visited = Array.fill(index.size)(false)
    val finished = Vector.newBuilder[Int]
    var start = 0
    while start < index.size do
      if !visited(start) then
        val stack = mutable.ArrayDeque((start, false))
        while stack.nonEmpty do
          val (current, expanded) = stack.removeLast()
          if expanded then finished += current
          else if !visited(current) then
            visited(current) = true
            stack.append((current, true))
            index.reverseSuccessors(current).foreach { next =>
              if !visited(next) then stack.append((next, false))
            }
      start += 1
    finished.result()

private[algorithms] trait DenseDirectedIndex[V] extends DenseForwardIndex[V]:
  def predecessors(vertex: Int): Iterator[Int]
  def reverseSuccessors(vertex: Int): Iterator[Int]
  def reversePredecessors(vertex: Int): Iterator[Int]
  def inDegree(vertex: Int): Int

private[algorithms] object DenseDirectedIndex:
  def apply[V](
      graph: Digraph[V],
      order: Order[V]
  ): DenseDirectedIndex[V] =
    IndexedDigraph
      .from(graph, VertexOrder.by(order))
      .toOption
      .fold(fallback(graph, order))(fromIndexed)

  private def fromIndexed[V](
      indexed: IndexedDigraph[V]
  ): DenseDirectedIndex[V] =
    new DenseDirectedIndex[V]:
      def size: Int = indexed.vertexCount
      def label(vertex: Int): V = indexed.labelAtOrdinal(vertex)
      def position(label: V): Option[Int] =
        indexed.vertex(label).map(indexed.ordinal)
      def successors(vertex: Int): Iterator[Int] =
        indexed.successorOrdinals(vertex)
      def predecessors(vertex: Int): Iterator[Int] =
        indexed.predecessorOrdinals(vertex)
      def reverseSuccessors(vertex: Int): Iterator[Int] =
        indexed.reverseSuccessorOrdinals(vertex)
      def reversePredecessors(vertex: Int): Iterator[Int] =
        indexed.reversePredecessorOrdinals(vertex)
      def inDegree(vertex: Int): Int =
        indexed.inDegreeAtOrdinal(vertex)

  /** Only used when a digraph exceeds the single-array CSR/CSC size limit. */
  private def fallback[V](
      graph: Digraph[V],
      order: Order[V]
  ): DenseDirectedIndex[V] =
    val index = OrderedIndex(graph.vertices, graph.vertexHash, order)
    new DenseDirectedIndex[V]:
      def size: Int = index.size
      def label(vertex: Int): V = index.label(vertex)
      def position(label: V): Option[Int] = index.position(label)
      def successors(vertex: Int): Iterator[Int] =
        successorIndices(vertex).iterator
      def predecessors(vertex: Int): Iterator[Int] =
        predecessorIndices(vertex).iterator
      def reverseSuccessors(vertex: Int): Iterator[Int] =
        successorIndices(vertex).reverseIterator
      def reversePredecessors(vertex: Int): Iterator[Int] =
        predecessorIndices(vertex).reverseIterator
      def inDegree(vertex: Int): Int =
        graph.inDegreeOf(index.label(vertex)).getOrElse(0)
      private def successorIndices(vertex: Int): Array[Int] =
        graph
          .successorsOf(index.label(vertex))
          .iterator
          .flatMap(_.iterator)
          .flatMap(index.position)
          .toArray
          .sorted
      private def predecessorIndices(vertex: Int): Array[Int] =
        graph
          .predecessorsOf(index.label(vertex))
          .iterator
          .flatMap(_.iterator)
          .flatMap(index.position)
          .toArray
          .sorted
