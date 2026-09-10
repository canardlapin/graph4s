package graph4s.indexed

import cats.Hash
import cats.collections.HashMap
import cats.data.ValidatedNec
import cats.syntax.all.*
import graph4s.{Graph, Link, MutableHashSet}

/** An immutable CSR snapshot of an undirected graph.
  *
  * `Vertex` and `Edge` are path-dependent opaque types: IDs from two indexed graph values cannot be
  * mixed, even though both erase to `Int`.
  */
final class IndexedGraph[V] private (
    private val labels: Vector[V],
    private val labelToIndex: HashMap[V, Int],
    private val offsets: Array[Int],
    private val neighborData: Array[Int],
    private val numberOfEdges: Int
)(using val vertexHash: Hash[V])
    extends IndexedVertexDomain[V]:

  opaque type Vertex = Int
  opaque type Edge = Int

  final class VertexSlice private[IndexedGraph] (
      private val start: Int,
      private val end: Int
  ):
    def size: Int = end - start
    def isEmpty: Boolean = start == end
    def nonEmpty: Boolean = start != end

    def get(index: Int): Option[Vertex] =
      if index >= 0 && index < size then Some(neighborData(start + index))
      else None

    def iterator: Iterator[Vertex] =
      (start until end).iterator.map(neighborData(_))

    def reverseIterator: Iterator[Vertex] =
      (end - 1 to start by -1).iterator.map(neighborData(_))

  def vertexCount: Int = labels.size
  def edgeCount: Int = numberOfEdges

  def vertex(label: V): Option[Vertex] =
    labelToIndex.get(label)

  /** The dense zero-based ordinal for this graph-scoped vertex.
    *
    * The returned `Int` is intentionally an explicit escape from graph scoping for primitive arrays
    * and numerical kernels. It is meaningful only for this indexed snapshot.
    */
  def ordinal(vertex: Vertex): Int =
    vertex

  /** Safely re-enters this graph's scoped vertex domain. */
  def vertexAt(ordinal: Int): Option[Vertex] =
    if ordinal >= 0 && ordinal < vertexCount then Some(ordinal) else None

  def label(vertex: Vertex): V =
    labels(vertex)

  private[graph4s] def labelAtOrdinal(ordinal: Int): V =
    labels(ordinal)

  def vertices: Iterator[Vertex] =
    labels.indices.iterator

  def edges: Iterator[Edge] =
    (0 until numberOfEdges).iterator

  def neighbors(vertex: Vertex): VertexSlice =
    new VertexSlice(offsets(vertex), offsets(vertex + 1))

  private[graph4s] def neighborOrdinals(vertex: Int): Iterator[Int] =
    (offsets(vertex) until offsets(vertex + 1)).iterator.map(neighborData(_))

  private[graph4s] def reverseNeighborOrdinals(vertex: Int): Iterator[Int] =
    (offsets(vertex + 1) - 1 to offsets(vertex) by -1).iterator.map(neighborData(_))

  def degree(vertex: Vertex): Int =
    offsets(vertex + 1) - offsets(vertex)

  def endpoints(edge: Edge): (Vertex, Vertex) =
    (first(edge), second(edge))

  def first(edge: Edge): Vertex =
    edgeIndex._1(edge)

  def second(edge: Edge): Vertex =
    edgeIndex._2(edge)

  def edgeOrdinal(edge: Edge): Int =
    edge

  def toGraph: Graph[V] =
    val (sources, targets) = edgeIndex
    Graph.materialize(
      labels,
      sources.indices.iterator.map(index => Link(labels(sources(index)), labels(targets(index))))
    )

  private lazy val edgeIndex: (Array[Int], Array[Int]) =
    val sources = Array.ofDim[Int](numberOfEdges)
    val targets = Array.ofDim[Int](numberOfEdges)
    var edgeCursor = 0
    var source = 0
    while source < labels.size do
      var position = offsets(source)
      while position < offsets(source + 1) do
        val target = neighborData(position)
        if source < target then
          sources(edgeCursor) = source
          targets(edgeCursor) = target
          edgeCursor += 1
        position += 1
      source += 1
    (sources, targets)

object IndexedGraph:
  def from[V](
      graph: Graph[V],
      order: VertexOrder[V]
  ): ValidatedNec[IndexingError[V], IndexedGraph[V]] =
    orderedLabels(graph, order).andThen(labels => build(graph, labels))

  private def orderedLabels[V](
      graph: Graph[V],
      policy: VertexOrder[V]
  ): ValidatedNec[IndexingError[V], Vector[V]] =
    policy match
      case VertexOrder.By(order) =>
        graph.vertices.sorted(using order).validNec
      case VertexOrder.Explicit(labels) =>
        val seen = new MutableHashSet[V](using graph.vertexHash)
        val errors = Vector.newBuilder[IndexingError[V]]
        labels.foreach { label =>
          if !seen.add(label) then errors += IndexingError.DuplicateVertex(label)
          if !graph.containsVertex(label) then errors += IndexingError.UnknownVertex(label)
        }
        graph.vertices.iterator.foreach { label =>
          if !seen.contains(label) then errors += IndexingError.MissingVertex(label)
        }
        cats.data.NonEmptyChain.fromSeq(errors.result()) match
          case Some(value) => value.invalid
          case None        => labels.valid

  private def build[V](
      graph: Graph[V],
      labels: Vector[V]
  ): ValidatedNec[IndexingError[V], IndexedGraph[V]] =
    val adjacencyEntries = graph.edgeCount * 2L
    IndexingLimits.arrayLength[V](adjacencyEntries) match
      case Left(error) =>
        error.invalidNec
      case Right(adjacencySize) =>
        given Hash[V] = graph.vertexHash
        val labelToIndex =
          HashMap.fromIterableOnce(labels.iterator.zipWithIndex)
        val offsets = Array.ofDim[Int](labels.size + 1)
        var adjacencyCursor = 0
        var vertex = 0
        while vertex < labels.size do
          offsets(vertex) = adjacencyCursor
          adjacencyCursor += graph.degreeOf(labels(vertex)).getOrElse(0)
          vertex += 1
        offsets(labels.size) = adjacencyCursor

        val neighbors = Array.ofDim[Int](adjacencySize)
        val cursors = offsets.clone()
        var inconsistentVertex: Option[V] = None
        graph.edges.iterator.foreach { edge =>
          (labelToIndex.get(edge.first), labelToIndex.get(edge.second)) match
            case (Some(source), Some(target)) =>
              neighbors(cursors(source)) = target
              cursors(source) += 1
              neighbors(cursors(target)) = source
              cursors(target) += 1
            case (None, _) =>
              inconsistentVertex = Some(edge.first)
            case (_, None) =>
              inconsistentVertex = Some(edge.second)
        }
        inconsistentVertex match
          case Some(vertex) =>
            IndexingError.InconsistentTopology(vertex).invalidNec
          case None =>
            vertex = 0
            while vertex < labels.size do
              java.util.Arrays.sort(
                neighbors,
                offsets(vertex),
                offsets(vertex + 1)
              )
              vertex += 1

            new IndexedGraph(
              labels,
              labelToIndex,
              offsets,
              neighbors,
              graph.edgeCount.toInt
            ).validNec
