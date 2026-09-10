package graph4s

import cats.{Hash, PartialOrder, Show}
import cats.collections.{HashMap, HashSet}
import cats.data.ValidatedNec
import cats.kernel.BoundedSemilattice
import cats.syntax.all.*

/** A finite, simple, undirected, loopless graph.
  *
  * Parallel edges have set semantics, all edge endpoints are vertices, and adjacency is symmetric.
  * Values are persistent and safe to share.
  */
final class Graph[V] private[graph4s] (
    private[graph4s] val adjacency: HashMap[V, HashSet[V]],
    private val vertexSet: VertexSet[V],
    val edgeCount: Long
)(using val vertexHash: Hash[V])
    extends VertexDomain[V]:

  def vertexCount: Int = vertexSet.size
  def vertices: VertexSet[V] = vertexSet

  def edges: Iterable[Edge[V]] =
    new Iterable[Edge[V]]:
      def iterator: Iterator[Edge[V]] =
        var visited = HashSet.empty[V](using vertexHash)
        adjacency.iterator.flatMap { case (vertex, neighbors) =>
          val seen = visited
          val fresh =
            neighbors.iterator
              .filterNot(seen.contains)
              .map(Edge.unsafe(vertex, _))
          visited = visited.add(vertex)
          fresh
        }

  def containsVertex(vertex: V): Boolean =
    vertexSet.contains(vertex)

  def containsEdge(left: V, right: V): Boolean =
    adjacency.get(left).exists(_.contains(right))

  def neighborsOf(vertex: V): Option[VertexSet[V]] =
    adjacency.get(vertex).map(VertexSet.unsafe)

  def degreeOf(vertex: V): Option[Int] =
    adjacency.get(vertex).map(_.size)

  def addVertex(vertex: V): Graph[V] =
    if containsVertex(vertex) then this
    else
      new Graph(
        adjacency.updated(vertex, HashSet.empty[V](using vertexHash)),
        vertexSet.add(vertex),
        edgeCount
      )

  def removeVertex(vertex: V): Graph[V] =
    adjacency.get(vertex) match
      case None            => this
      case Some(neighbors) =>
        val withoutIncident =
          neighbors.iterator.foldLeft(adjacency) { (current, neighbor) =>
            current.get(neighbor) match
              case Some(values) =>
                current.updated(neighbor, values.remove(vertex))
              case None =>
                current
          }
        new Graph(
          withoutIncident.removed(vertex),
          vertexSet.remove(vertex),
          edgeCount - neighbors.size.toLong
        )

  /** Connect two vertices, inserting missing endpoints. */
  def connect(left: V, right: V): Either[ConnectError.SelfLoop[V], Graph[V]] =
    if vertexHash.eqv(left, right) then Left(ConnectError.SelfLoop(left))
    else Right(connectUnchecked(left, right))

  /** Connect two vertices only when both already belong to this graph. */
  def connectExisting(left: V, right: V): Either[ConnectError[V], Graph[V]] =
    if vertexHash.eqv(left, right) then Left(ConnectError.SelfLoop(left))
    else
      val missing =
        Vector(left, right).filterNot(containsVertex)
      cats.data.NonEmptyChain.fromSeq(missing) match
        case Some(vertices) => Left(ConnectError.MissingEndpoints(vertices))
        case None           => Right(connectUnchecked(left, right))

  def disconnect(left: V, right: V): Graph[V] =
    if !containsEdge(left, right) then this
    else
      val leftNeighbors = adjacency.getOrElse(left, HashSet.empty[V](using vertexHash))
      val rightNeighbors = adjacency.getOrElse(right, HashSet.empty[V](using vertexHash))
      new Graph(
        adjacency
          .updated(left, leftNeighbors.remove(right))
          .updated(right, rightNeighbors.remove(left)),
        vertexSet,
        edgeCount - 1L
      )

  /** Set union of vertices and edges. */
  def overlay(other: Graph[V]): Graph[V] =
    GraphBuilder.overlay(this, other)

  def inducedBy(predicate: V => Boolean): InducedSubgraph[V] =
    induced(vertexSet.filter(predicate))

  def induced(selected: IterableOnce[V]): InducedSubgraph[V] =
    induced(VertexSet.from(selected)(using vertexHash).intersect(vertexSet))

  private def induced(selected: VertexSet[V]): InducedSubgraph[V] =
    val keptEdges =
      edges.iterator.filter(edge => selected.contains(edge.first) && selected.contains(edge.second))
    val result =
      Graph.materialize(selected.iterator, keptEdges.map(e => Link(e.first, e.second)))(using
        vertexHash
      )
    InducedSubgraph(this, result, vertexSet.diff(selected))

  def spanningBy(predicate: Edge[V] => Boolean): Graph[V] =
    Graph.materialize(
      vertices.iterator,
      edges.iterator.filter(predicate).map(edge => Link(edge.first, edge.second))
    )(using vertexHash)

  def complement: Graph[V] =
    GraphBuilder.complement(this)

  def disjointUnion[W](other: Graph[W]): Graph[Either[V, W]] =
    val resultHash = InternalHash.either(vertexHash, other.vertexHash)
    val resultVertices =
      vertices.iterator.map(Left(_)) ++ other.vertices.iterator.map(Right(_))
    val resultEdges =
      edges.iterator.map(e => Link(Left(e.first), Left(e.second))) ++
        other.edges.iterator.map(e => Link(Right(e.first), Right(e.second)))
    Graph.materialize(resultVertices, resultEdges)(using resultHash)

  def join[W](other: Graph[W]): Graph[Either[V, W]] =
    val resultHash = InternalHash.either(vertexHash, other.vertexHash)
    val resultVertices =
      vertices.iterator.map(Left(_)) ++ other.vertices.iterator.map(Right(_))
    val internalEdges =
      edges.iterator.map(e => Link(Left(e.first), Left(e.second))) ++
        other.edges.iterator.map(e => Link(Right(e.first), Right(e.second)))
    val crossEdges =
      vertices.iterator.flatMap(left =>
        other.vertices.iterator.map(right => Link(Left(left), Right(right)))
      )
    Graph.materialize(resultVertices, internalEdges ++ crossEdges)(using resultHash)

  def cartesianProduct[W](other: Graph[W]): Graph[(V, W)] =
    val resultHash = InternalHash.tuple2(vertexHash, other.vertexHash)
    val resultVertices =
      vertices.iterator.flatMap(left => other.vertices.iterator.map((left, _)))
    val fromLeft =
      edges.iterator.flatMap(edge =>
        other.vertices.iterator.map(right => Link((edge.first, right), (edge.second, right)))
      )
    val fromRight =
      vertices.iterator.flatMap(left =>
        other.edges.iterator.map(edge => Link((left, edge.first), (left, edge.second)))
      )
    Graph.materialize(resultVertices, fromLeft ++ fromRight)(using resultHash)

  def lineGraph: Graph[Edge[V]] =
    given Hash[V] = vertexHash
    val edgeHash = summon[Hash[Edge[V]]]
    val allEdges = edges.toVector
    val incidence = MutableHashMap[V, scala.collection.mutable.ArrayBuffer[Edge[V]]]()
    vertices.iterator.foreach { vertex =>
      incidence.put(vertex, scala.collection.mutable.ArrayBuffer.empty)
    }
    allEdges.foreach { edge =>
      incidence.get(edge.first).foreach(_ += edge)
      incidence.get(edge.second).foreach(_ += edge)
    }
    val lineEdges =
      vertices.iterator.flatMap { vertex =>
        val incident =
          incidence
            .get(vertex)
            .fold(Vector.empty[Edge[V]])(_.toVector)
        for
          leftIndex <- incident.indices.iterator
          rightIndex <- (leftIndex + 1 until incident.size).iterator
        yield Link(incident(leftIndex), incident(rightIndex))
      }
    Graph.materialize(allEdges, lineEdges)(using edgeHash)

  def bidirected: Digraph[V] =
    val arcs =
      edges.iterator.flatMap(edge =>
        Iterator(
          ArcInput(edge.first, edge.second),
          ArcInput(edge.second, edge.first)
        )
      )
    Digraph.materialize(vertices.iterator, arcs)(using vertexHash)

  private[graph4s] def isSubgraphOf(other: Graph[V]): Boolean =
    vertices.iterator.forall(other.containsVertex) &&
      edges.iterator.forall(edge => other.containsEdge(edge.first, edge.second))

  private def connectUnchecked(left: V, right: V): Graph[V] =
    val withLeft = addVertex(left)
    val withBoth = withLeft.addVertex(right)
    if withBoth.containsEdge(left, right) then withBoth
    else
      val leftNeighbors =
        withBoth.adjacency.getOrElse(left, HashSet.empty[V](using vertexHash))
      val rightNeighbors =
        withBoth.adjacency.getOrElse(right, HashSet.empty[V](using vertexHash))
      new Graph(
        withBoth.adjacency
          .updated(left, leftNeighbors.add(right))
          .updated(right, rightNeighbors.add(left)),
        withBoth.vertexSet,
        withBoth.edgeCount + 1L
      )

object Graph:
  def empty[V: Hash]: Graph[V] =
    new Graph(
      HashMap.empty[V, HashSet[V]],
      VertexSet.empty[V],
      0L
    )

  def singleton[V: Hash](vertex: V): Graph[V] =
    empty[V].addVertex(vertex)

  def of[V: Hash](
      vertices: IterableOnce[V],
      edges: IterableOnce[Link[V]]
  ): ValidatedNec[GraphBuildError[V], Graph[V]] =
    GraphBuilder.authoritative(vertices, edges)

  def fromEdges[V: Hash](
      edges: IterableOnce[Link[V]]
  ): ValidatedNec[GraphBuildError[V], Graph[V]] =
    GraphBuilder.inferred(edges)

  def fromEdges[V: Hash](
      edges: Link[V]*
  ): ValidatedNec[GraphBuildError[V], Graph[V]] =
    fromEdges(edges: IterableOnce[Link[V]])

  private[graph4s] def materialize[V: Hash](
      vertices: IterableOnce[V],
      edges: IterableOnce[Link[V]]
  ): Graph[V] =
    GraphBuilder.materialize(vertices, edges)

  given [V: Show]: Show[Graph[V]] =
    Show.show { graph =>
      val shownVertices =
        graph.vertices.iterator.map(Show[V].show).mkString("(", ", ", ")")
      val shownEdges =
        graph.edges.iterator.map(Show[Edge[V]].show).mkString("(", ", ", ")")
      s"Graph(vertices = $shownVertices, edges = $shownEdges)"
    }

  given [V]: PartialOrder[Graph[V]] with
    override def eqv(left: Graph[V], right: Graph[V]): Boolean =
      left.vertexCount == right.vertexCount &&
        left.edgeCount == right.edgeCount &&
        left.isSubgraphOf(right)

    def partialCompare(left: Graph[V], right: Graph[V]): Double =
      val leftBelow = left.isSubgraphOf(right)
      val rightBelow = right.isSubgraphOf(left)
      if leftBelow && rightBelow then 0.0
      else if leftBelow then -1.0
      else if rightBelow then 1.0
      else Double.NaN

  given [V: Hash]: BoundedSemilattice[Graph[V]] with
    def empty: Graph[V] = Graph.empty[V]
    def combine(left: Graph[V], right: Graph[V]): Graph[V] =
      left.overlay(right)

final case class InducedSubgraph[V](
    source: Graph[V],
    graph: Graph[V],
    omitted: VertexSet[V]
)

private object GraphBuilder:
  private final class State[V](using V: Hash[V]):
    private val vertices = MutableHashSet[V]()
    private val adjacency = MutableHashMap[V, MutableHashSet[V]]()
    private var edges = 0L

    def addVertex(vertex: V): Unit =
      if vertices.add(vertex) then adjacency.put(vertex, MutableHashSet[V]())

    def containsVertex(vertex: V): Boolean =
      vertices.contains(vertex)

    def addEdge(left: V, right: V): Unit =
      val leftNeighbors = adjacency.getOrElseUpdate(left, MutableHashSet[V]())
      val rightNeighbors = adjacency.getOrElseUpdate(right, MutableHashSet[V]())
      if leftNeighbors.add(right) then
        rightNeighbors.add(left)
        edges += 1L

    def freeze: Graph[V] =
      val frozenAdjacency =
        HashMap.fromIterableOnce(
          adjacency.iterator.map { case (vertex, neighbors) =>
            (vertex, neighbors.freeze)
          }
        )
      new Graph(frozenAdjacency, VertexSet.unsafe(vertices.freeze), edges)

  def authoritative[V: Hash](
      suppliedVertices: IterableOnce[V],
      suppliedEdges: IterableOnce[Link[V]]
  ): ValidatedNec[GraphBuildError[V], Graph[V]] =
    val V = Hash[V]
    val state = State[V]()
    suppliedVertices.iterator.foreach(state.addVertex)
    val errors = Vector.newBuilder[GraphBuildError[V]]

    suppliedEdges.iterator.foreach { edge =>
      val loop = V.eqv(edge.left, edge.right)
      val leftKnown = state.containsVertex(edge.left)
      val rightKnown = state.containsVertex(edge.right)

      if loop then errors += GraphBuildError.SelfLoop(edge.left)
      if !leftKnown then errors += GraphBuildError.UnknownEndpoint(edge, edge.left)
      if !rightKnown && !V.eqv(edge.left, edge.right) then
        errors += GraphBuildError.UnknownEndpoint(edge, edge.right)
      if !loop && leftKnown && rightKnown then state.addEdge(edge.left, edge.right)
    }

    cats.data.NonEmptyChain.fromSeq(errors.result()) match
      case Some(value) => value.invalid
      case None        => state.freeze.valid

  def inferred[V: Hash](
      suppliedEdges: IterableOnce[Link[V]]
  ): ValidatedNec[GraphBuildError[V], Graph[V]] =
    val V = Hash[V]
    val state = State[V]()
    val errors = Vector.newBuilder[GraphBuildError[V]]

    suppliedEdges.iterator.foreach { edge =>
      state.addVertex(edge.left)
      state.addVertex(edge.right)
      if V.eqv(edge.left, edge.right) then errors += GraphBuildError.SelfLoop(edge.left)
      else state.addEdge(edge.left, edge.right)
    }

    cats.data.NonEmptyChain.fromSeq(errors.result()) match
      case Some(value) => value.invalid
      case None        => state.freeze.valid

  def materialize[V: Hash](
      suppliedVertices: IterableOnce[V],
      suppliedEdges: IterableOnce[Link[V]]
  ): Graph[V] =
    val V = Hash[V]
    val state = State[V]()
    suppliedVertices.iterator.foreach(state.addVertex)
    suppliedEdges.iterator.foreach { edge =>
      if !V.eqv(edge.left, edge.right) then
        state.addVertex(edge.left)
        state.addVertex(edge.right)
        state.addEdge(edge.left, edge.right)
    }
    state.freeze

  def overlay[V](left: Graph[V], right: Graph[V]): Graph[V] =
    given Hash[V] = left.vertexHash
    val state = State[V]()
    left.vertices.iterator.foreach(state.addVertex)
    right.vertices.iterator.foreach(state.addVertex)
    left.edges.iterator.foreach(edge => state.addEdge(edge.first, edge.second))
    right.edges.iterator.foreach(edge => state.addEdge(edge.first, edge.second))
    state.freeze

  def complement[V](graph: Graph[V]): Graph[V] =
    given Hash[V] = graph.vertexHash
    val labels = graph.vertices.toVector
    val complementAdjacency =
      HashMap.fromIterableOnce(
        labels.iterator.map { vertex =>
          val existing =
            graph.adjacency.getOrElse(vertex, HashSet.empty[V])
          val neighbors =
            HashSet.fromIterableOnce(
              labels.iterator.filter(candidate =>
                !graph.vertexHash.eqv(vertex, candidate) &&
                  !existing.contains(candidate)
              )
            )
          (vertex, neighbors)
        }
      )
    val completeEdgeCount =
      labels.size.toLong * (labels.size.toLong - 1L) / 2L
    new Graph(
      complementAdjacency,
      graph.vertices,
      completeEdgeCount - graph.edgeCount
    )
