package graph4s.algorithms

import cats.{Hash, Order}
import cats.collections.HashMap
import graph4s.data.VertexMap
import graph4s.indexed.{IndexedGraph, VertexOrder}
import graph4s.{ConnectivityError, Graph, MissingVertex, VertexDomain, VertexSet}
import scala.collection.mutable

object GraphAlgorithms:
  def bfs[V](
      graph: Graph[V],
      root: V
  )(using order: Order[V]): Either[MissingVertex[V], BfsResult[V]] =
    if !graph.containsVertex(root) then Left(MissingVertex.Vertex(root))
    else
      val index = DenseUndirectedIndex(graph, order)
      index.position(root) match
        case None        => Left(MissingVertex.Vertex(root))
        case Some(start) => Right(DenseTraversal.bfs(graph, index, start))

  def dfs[V](
      graph: Graph[V],
      root: V
  )(using order: Order[V]): Either[MissingVertex[V], DfsResult[V]] =
    if !graph.containsVertex(root) then Left(MissingVertex.Vertex(root))
    else
      val index = DenseUndirectedIndex(graph, order)
      index.position(root) match
        case None =>
          Left(MissingVertex.Vertex(root))
        case Some(start) =>
          val visited = Array.fill(index.size)(false)
          // Sentinels are confined to private primitive work arrays.
          val parent = Array.fill(index.size)(-1)
          val stack = mutable.ArrayDeque((start, -1))
          val preorder = Vector.newBuilder[V]

          while stack.nonEmpty do
            val (current, discoverer) = stack.removeLast()
            if !visited(current) then
              visited(current) = true
              if discoverer >= 0 then parent(current) = discoverer
              preorder += index.label(current)
              index.reverseSuccessors(current).foreach { next =>
                if !visited(next) then stack.append((next, current))
              }

          val parentEntries =
            visited.indices.iterator.filter(visited).map { vertex =>
              val parentLabel =
                if parent(vertex) < 0 then None
                else Some(index.label(parent(vertex)))
              (index.label(vertex), parentLabel)
            }
          Right(
            DfsResult(
              root,
              preorder.result(),
              VertexMap.fromTrusted(graph, parentEntries)
            )
          )

  def shortestPath[V](
      graph: Graph[V],
      source: V,
      target: V
  )(using order: Order[V]): Either[MissingVertex[V], Option[Path[V]]] =
    missingEndpoints(graph, source, target) match
      case Some(error) =>
        Left(error)
      case None =>
        val index = DenseUndirectedIndex(graph, order)
        (index.position(source), index.position(target)) match
          case (Some(start), Some(end)) =>
            Right(shortestPath(index, start, end))
          case (None, _) =>
            Left(MissingVertex.Vertex(source))
          case (_, None) =>
            Left(MissingVertex.Vertex(target))

  def connectedComponents[V](
      graph: Graph[V]
  )(using order: Order[V]): Components[V] =
    components(graph, DenseUndirectedIndex(graph, order))

  def requireConnected[V](
      graph: Graph[V]
  )(using order: Order[V]): Either[ConnectivityError[V], ConnectedGraph[V]] =
    val index = DenseUndirectedIndex(graph, order)
    val result = components(graph, index)
    result.components match
      case Vector() =>
        Left(ConnectivityError.EmptyGraph[V]())
      case Vector(_) =>
        val traversal = DenseTraversal.bfs(graph, index, 0)
        Right(
          new ConnectedGraph(
            graph,
            SpanningTree(traversal.root, traversal.parent)
          )
        )
      case many =>
        Left(ConnectivityError.Disconnected(many))

  private def shortestPath[V](
      index: DenseUndirectedIndex[V],
      start: Int,
      target: Int
  ): Option[Path[V]] =
    if start == target then Some(Path.one(index.label(start)))
    else
      val visited = Array.fill(index.size)(false)
      val parent = Array.fill(index.size)(-1)
      val queue = mutable.ArrayDeque(start)
      var found = false
      visited(start) = true

      while queue.nonEmpty && !found do
        val current = queue.removeHead()
        index.successors(current).foreach { next =>
          if !visited(next) && !found then
            visited(next) = true
            parent(next) = current
            if next == target then found = true
            else queue.append(next)
        }

      if !found then None
      else
        val reversed = Vector.newBuilder[V]
        var current = target
        while current >= 0 do
          reversed += index.label(current)
          if current == start then current = -1
          else current = parent(current)
        Path.fromVertices(reversed.result().reverse)

  private def components[V](
      graph: Graph[V],
      index: DenseUndirectedIndex[V]
  ): Components[V] =
    val assigned = Array.fill(index.size)(false)
    val componentSets = Vector.newBuilder[VertexSet[V]]
    val membership = Vector.newBuilder[(V, Int)]
    var componentIndex = 0
    var start = 0

    while start < index.size do
      if !assigned(start) then
        val queue = mutable.ArrayDeque(start)
        val labels = Vector.newBuilder[V]
        assigned(start) = true
        while queue.nonEmpty do
          val current = queue.removeHead()
          val label = index.label(current)
          labels += label
          membership += ((label, componentIndex))
          index.successors(current).foreach { next =>
            if !assigned(next) then
              assigned(next) = true
              queue.append(next)
          }
        componentSets += VertexSet.from(labels.result())(using graph.vertexHash)
        componentIndex += 1
      start += 1

    Components(
      componentSets.result(),
      VertexMap.fromTrusted(graph, membership.result())
    )

  private def missingEndpoints[V](
      graph: Graph[V],
      source: V,
      target: V
  ): Option[MissingVertex[V]] =
    val candidates =
      if graph.vertexHash.eqv(source, target) then Vector(source)
      else Vector(source, target)
    val missing = candidates.filterNot(graph.containsVertex)
    cats.data.NonEmptyChain.fromSeq(missing).map { values =>
      if values.tail.isEmpty then MissingVertex.Vertex(values.head)
      else MissingVertex.Vertices(values)
    }

private[algorithms] trait DenseVertexIndex[V]:
  def size: Int
  def label(vertex: Int): V
  def position(label: V): Option[Int]

private[algorithms] trait DenseForwardIndex[V] extends DenseVertexIndex[V]:
  def successors(vertex: Int): Iterator[Int]

private[algorithms] object DenseTraversal:
  def bfs[V](
      topology: VertexDomain[V],
      index: DenseForwardIndex[V],
      start: Int
  ): BfsResult[V] =
    val visited = Array.fill(index.size)(false)
    // Sentinels are confined to private primitive work arrays.
    val distance = Array.fill(index.size)(-1)
    val parent = Array.fill(index.size)(-1)
    val queue = mutable.ArrayDeque(start)
    val traversal = Vector.newBuilder[V]
    visited(start) = true
    distance(start) = 0

    while queue.nonEmpty do
      val current = queue.removeHead()
      traversal += index.label(current)
      index.successors(current).foreach { next =>
        if !visited(next) then
          visited(next) = true
          distance(next) = distance(current) + 1
          parent(next) = current
          queue.append(next)
      }

    val reached = visited.indices.iterator.filter(visited).toVector
    val distances = reached.iterator.map(vertex => (index.label(vertex), distance(vertex)))
    val parents = reached.iterator.map { vertex =>
      val parentLabel =
        if parent(vertex) < 0 then None
        else Some(index.label(parent(vertex)))
      (index.label(vertex), parentLabel)
    }
    BfsResult(
      index.label(start),
      traversal.result(),
      VertexMap.fromTrusted(topology, distances),
      VertexMap.fromTrusted(topology, parents)
    )

private[algorithms] trait DenseUndirectedIndex[V] extends DenseForwardIndex[V]:
  def reverseSuccessors(vertex: Int): Iterator[Int]

private[algorithms] object DenseUndirectedIndex:
  def apply[V](
      graph: Graph[V],
      order: Order[V]
  ): DenseUndirectedIndex[V] =
    IndexedGraph
      .from(graph, VertexOrder.by(order))
      .toOption
      .fold(fallback(graph, order))(fromIndexed)

  private def fromIndexed[V](
      indexed: IndexedGraph[V]
  ): DenseUndirectedIndex[V] =
    new DenseUndirectedIndex[V]:
      def size: Int = indexed.vertexCount
      def label(vertex: Int): V = indexed.labelAtOrdinal(vertex)
      def position(label: V): Option[Int] =
        indexed.vertex(label).map(indexed.ordinal)
      def successors(vertex: Int): Iterator[Int] =
        indexed.neighborOrdinals(vertex)
      def reverseSuccessors(vertex: Int): Iterator[Int] =
        indexed.reverseNeighborOrdinals(vertex)

  /** Only used when a graph exceeds the single-array CSR size limit. */
  private def fallback[V](
      graph: Graph[V],
      order: Order[V]
  ): DenseUndirectedIndex[V] =
    val index = OrderedIndex(graph.vertices, graph.vertexHash, order)
    new DenseUndirectedIndex[V]:
      def size: Int = index.size
      def label(vertex: Int): V = index.label(vertex)
      def position(label: V): Option[Int] = index.position(label)
      def successors(vertex: Int): Iterator[Int] =
        neighborIndices(vertex).iterator
      def reverseSuccessors(vertex: Int): Iterator[Int] =
        neighborIndices(vertex).reverseIterator
      private def neighborIndices(vertex: Int): Array[Int] =
        graph
          .neighborsOf(index.label(vertex))
          .iterator
          .flatMap(_.iterator)
          .flatMap(index.position)
          .toArray
          .sorted

private[algorithms] final class OrderedIndex[V] private (
    private val labels: Vector[V],
    private val positions: HashMap[V, Int]
):
  def size: Int = labels.size
  def label(index: Int): V = labels(index)
  def position(label: V): Option[Int] = positions.get(label)

private[algorithms] object OrderedIndex:
  def apply[V](
      vertices: VertexSet[V],
      hash: Hash[V],
      order: Order[V]
  ): OrderedIndex[V] =
    given Hash[V] = hash
    val labels = vertices.sorted(using order)
    new OrderedIndex(
      labels,
      HashMap.fromIterableOnce(labels.iterator.zipWithIndex)
    )
