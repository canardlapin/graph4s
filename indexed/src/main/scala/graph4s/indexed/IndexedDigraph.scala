package graph4s.indexed

import cats.Hash
import cats.collections.HashMap
import cats.data.ValidatedNec
import cats.syntax.all.*
import graph4s.{ArcInput, Digraph, MutableHashSet}

/** An immutable CSR/CSC snapshot of a directed graph. */
final class IndexedDigraph[V] private (
    private val labels: Vector[V],
    private val labelToIndex: HashMap[V, Int],
    private val outOffsets: Array[Int],
    private val outTargets: Array[Int],
    private val inOffsets: Array[Int],
    private val inSources: Array[Int],
    private val numberOfArcs: Int
)(using val vertexHash: Hash[V]):

  opaque type Vertex = Int
  opaque type Arc = Int

  final class VertexSlice private[IndexedDigraph] (
      private val data: Array[Int],
      private val start: Int,
      private val end: Int
  ):
    def size: Int = end - start
    def isEmpty: Boolean = start == end
    def nonEmpty: Boolean = start != end

    def get(index: Int): Option[Vertex] =
      if index >= 0 && index < size then Some(data(start + index))
      else None

    def iterator: Iterator[Vertex] =
      (start until end).iterator.map(data(_))

    def reverseIterator: Iterator[Vertex] =
      (end - 1 to start by -1).iterator.map(data(_))

  def vertexCount: Int = labels.size
  def arcCount: Int = numberOfArcs
  def edgeCount: Int = arcCount

  def vertex(label: V): Option[Vertex] =
    labelToIndex.get(label)

  def ordinal(vertex: Vertex): Int =
    vertex

  def vertexAt(ordinal: Int): Option[Vertex] =
    if ordinal >= 0 && ordinal < vertexCount then Some(ordinal) else None

  def label(vertex: Vertex): V =
    labels(vertex)

  private[graph4s] def labelAtOrdinal(ordinal: Int): V =
    labels(ordinal)

  def vertices: Iterator[Vertex] =
    labels.indices.iterator

  def arcs: Iterator[Arc] =
    (0 until numberOfArcs).iterator

  def successors(vertex: Vertex): VertexSlice =
    new VertexSlice(outTargets, outOffsets(vertex), outOffsets(vertex + 1))

  def predecessors(vertex: Vertex): VertexSlice =
    new VertexSlice(inSources, inOffsets(vertex), inOffsets(vertex + 1))

  private[graph4s] def successorOrdinals(vertex: Int): Iterator[Int] =
    (outOffsets(vertex) until outOffsets(vertex + 1)).iterator.map(outTargets(_))

  private[graph4s] def predecessorOrdinals(vertex: Int): Iterator[Int] =
    (inOffsets(vertex) until inOffsets(vertex + 1)).iterator.map(inSources(_))

  private[graph4s] def reverseSuccessorOrdinals(vertex: Int): Iterator[Int] =
    (outOffsets(vertex + 1) - 1 to outOffsets(vertex) by -1).iterator.map(outTargets(_))

  private[graph4s] def reversePredecessorOrdinals(vertex: Int): Iterator[Int] =
    (inOffsets(vertex + 1) - 1 to inOffsets(vertex) by -1).iterator.map(inSources(_))

  def outDegree(vertex: Vertex): Int =
    outOffsets(vertex + 1) - outOffsets(vertex)

  def inDegree(vertex: Vertex): Int =
    inOffsets(vertex + 1) - inOffsets(vertex)

  private[graph4s] def inDegreeAtOrdinal(vertex: Int): Int =
    inOffsets(vertex + 1) - inOffsets(vertex)

  def endpoints(arc: Arc): (Vertex, Vertex) =
    val (sources, targets) = arcIndex
    (sources(arc), targets(arc))

  def arcOrdinal(arc: Arc): Int =
    arc

  def toDigraph: Digraph[V] =
    val (sources, targets) = arcIndex
    Digraph.materialize(
      labels,
      sources.indices.iterator.map(index =>
        ArcInput(labels(sources(index)), labels(targets(index)))
      )
    )

  private lazy val arcIndex: (Array[Int], Array[Int]) =
    val sources = Array.ofDim[Int](numberOfArcs)
    val targets = Array.ofDim[Int](numberOfArcs)
    var cursor = 0
    var source = 0
    while source < labels.size do
      var position = outOffsets(source)
      while position < outOffsets(source + 1) do
        sources(cursor) = source
        targets(cursor) = outTargets(position)
        cursor += 1
        position += 1
      source += 1
    (sources, targets)

object IndexedDigraph:
  def from[V](
      graph: Digraph[V],
      order: VertexOrder[V]
  ): ValidatedNec[IndexingError[V], IndexedDigraph[V]] =
    orderedLabels(graph, order).andThen(labels => build(graph, labels))

  private def orderedLabels[V](
      graph: Digraph[V],
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
      graph: Digraph[V],
      labels: Vector[V]
  ): ValidatedNec[IndexingError[V], IndexedDigraph[V]] =
    IndexingLimits.arrayLength[V](graph.arcCount) match
      case Left(error) =>
        error.invalidNec
      case Right(arcCount) =>
        given Hash[V] = graph.vertexHash
        val labelToIndex =
          HashMap.fromIterableOnce(labels.iterator.zipWithIndex)

        def offsets(degree: V => Option[Int]): Array[Int] =
          val offsets = Array.ofDim[Int](labels.size + 1)
          var cursor = 0
          var index = 0
          while index < labels.size do
            offsets(index) = cursor
            cursor += degree(labels(index)).getOrElse(0)
            index += 1
          offsets(labels.size) = cursor
          offsets

        val outOffsets = offsets(graph.outDegreeOf)
        val inOffsets = offsets(graph.inDegreeOf)
        val outTargets = Array.ofDim[Int](arcCount)
        val inSources = Array.ofDim[Int](arcCount)
        val outCursors = outOffsets.clone()
        val inCursors = inOffsets.clone()
        var inconsistentVertex: Option[V] = None
        graph.arcs.iterator.foreach { arc =>
          (labelToIndex.get(arc.source), labelToIndex.get(arc.target)) match
            case (Some(source), Some(target)) =>
              outTargets(outCursors(source)) = target
              outCursors(source) += 1
              inSources(inCursors(target)) = source
              inCursors(target) += 1
            case (None, _) =>
              inconsistentVertex = Some(arc.source)
            case (_, None) =>
              inconsistentVertex = Some(arc.target)
        }
        inconsistentVertex match
          case Some(vertex) =>
            IndexingError.InconsistentTopology(vertex).invalidNec
          case None =>
            var vertex = 0
            while vertex < labels.size do
              java.util.Arrays.sort(
                outTargets,
                outOffsets(vertex),
                outOffsets(vertex + 1)
              )
              java.util.Arrays.sort(
                inSources,
                inOffsets(vertex),
                inOffsets(vertex + 1)
              )
              vertex += 1

            new IndexedDigraph(
              labels,
              labelToIndex,
              outOffsets,
              outTargets,
              inOffsets,
              inSources,
              arcCount
            ).validNec
