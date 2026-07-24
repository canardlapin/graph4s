package graph4s

import cats.{Hash, PartialOrder, Show}
import cats.collections.{HashMap, HashSet}
import cats.data.ValidatedNec
import cats.kernel.BoundedSemilattice
import cats.syntax.all.*

/** A finite, simple, directed, loopless graph.
  *
  * Parallel arcs have set semantics. Every source and target is a graph vertex, and successor and
  * predecessor indexes are maintained together.
  */
final class Digraph[V] private[graph4s] (
    private[graph4s] val outgoing: HashMap[V, HashSet[V]],
    private[graph4s] val incoming: HashMap[V, HashSet[V]],
    private val vertexSet: VertexSet[V],
    val arcCount: Long
)(using val vertexHash: Hash[V])
    extends VertexDomain[V]:

  def vertexCount: Int = vertexSet.size
  def edgeCount: Long = arcCount
  def vertices: VertexSet[V] = vertexSet

  def arcs: Iterable[Arc[V]] =
    new Iterable[Arc[V]]:
      def iterator: Iterator[Arc[V]] =
        outgoing.iterator.flatMap { case (source, targets) =>
          targets.iterator.map(Arc.unsafe(source, _))
        }

  def containsVertex(vertex: V): Boolean =
    vertexSet.contains(vertex)

  def containsArc(source: V, target: V): Boolean =
    outgoing.get(source).exists(_.contains(target))

  def successorsOf(vertex: V): Option[VertexSet[V]] =
    outgoing.get(vertex).map(VertexSet.unsafe)

  def predecessorsOf(vertex: V): Option[VertexSet[V]] =
    incoming.get(vertex).map(VertexSet.unsafe)

  def outDegreeOf(vertex: V): Option[Int] =
    outgoing.get(vertex).map(_.size)

  def inDegreeOf(vertex: V): Option[Int] =
    incoming.get(vertex).map(_.size)

  def addVertex(vertex: V): Digraph[V] =
    if containsVertex(vertex) then this
    else
      val emptyNeighbors = HashSet.empty[V](using vertexHash)
      new Digraph(
        outgoing.updated(vertex, emptyNeighbors),
        incoming.updated(vertex, emptyNeighbors),
        vertexSet.add(vertex),
        arcCount
      )

  def removeVertex(vertex: V): Digraph[V] =
    (outgoing.get(vertex), incoming.get(vertex)) match
      case (Some(successors), Some(predecessors)) =>
        val withoutIncoming =
          successors.iterator.foldLeft(incoming) { (current, successor) =>
            current.get(successor) match
              case Some(values) =>
                current.updated(successor, values.remove(vertex))
              case None =>
                current
          }
        val withoutOutgoing =
          predecessors.iterator.foldLeft(outgoing) { (current, predecessor) =>
            current.get(predecessor) match
              case Some(values) =>
                current.updated(predecessor, values.remove(vertex))
              case None =>
                current
          }
        new Digraph(
          withoutOutgoing.removed(vertex),
          withoutIncoming.removed(vertex),
          vertexSet.remove(vertex),
          arcCount - successors.size.toLong - predecessors.size.toLong
        )
      case _ =>
        this

  def connect(source: V, target: V): Either[ConnectError.SelfLoop[V], Digraph[V]] =
    if vertexHash.eqv(source, target) then Left(ConnectError.SelfLoop(source))
    else Right(connectUnchecked(source, target))

  def connectExisting(source: V, target: V): Either[ConnectError[V], Digraph[V]] =
    if vertexHash.eqv(source, target) then Left(ConnectError.SelfLoop(source))
    else
      val missing = Vector(source, target).filterNot(containsVertex)
      cats.data.NonEmptyChain.fromSeq(missing) match
        case Some(vertices) => Left(ConnectError.MissingEndpoints(vertices))
        case None           => Right(connectUnchecked(source, target))

  def disconnect(source: V, target: V): Digraph[V] =
    if !containsArc(source, target) then this
    else
      val successors =
        outgoing.getOrElse(source, HashSet.empty[V](using vertexHash))
      val predecessors =
        incoming.getOrElse(target, HashSet.empty[V](using vertexHash))
      new Digraph(
        outgoing.updated(source, successors.remove(target)),
        incoming.updated(target, predecessors.remove(source)),
        vertexSet,
        arcCount - 1L
      )

  def overlay(other: Digraph[V]): Digraph[V] =
    DigraphBuilder.overlay(this, other)

  def transpose: Digraph[V] =
    new Digraph(incoming, outgoing, vertexSet, arcCount)

  def underlyingGraph: Graph[V] =
    Graph.materialize(
      vertices.iterator,
      arcs.iterator.map(arc => Link(arc.source, arc.target))
    )(using vertexHash)

  def inducedBy(predicate: V => Boolean): InducedSubgraphDigraph[V] =
    val selected = vertexSet.filter(predicate)
    val keptArcs =
      arcs.iterator.filter(arc => selected.contains(arc.source) && selected.contains(arc.target))
    val graph = Digraph.materialize(
      selected.iterator,
      keptArcs.map(arc => ArcInput(arc.source, arc.target))
    )(using vertexHash)
    InducedSubgraphDigraph(this, graph, vertexSet.diff(selected))

  private[graph4s] def isSubgraphOf(other: Digraph[V]): Boolean =
    vertices.iterator.forall(other.containsVertex) &&
      arcs.iterator.forall(arc => other.containsArc(arc.source, arc.target))

  private def connectUnchecked(source: V, target: V): Digraph[V] =
    val withSource = addVertex(source)
    val withBoth = withSource.addVertex(target)
    if withBoth.containsArc(source, target) then withBoth
    else
      val successors =
        withBoth.outgoing.getOrElse(source, HashSet.empty[V](using vertexHash))
      val predecessors =
        withBoth.incoming.getOrElse(target, HashSet.empty[V](using vertexHash))
      new Digraph(
        withBoth.outgoing.updated(source, successors.add(target)),
        withBoth.incoming.updated(target, predecessors.add(source)),
        withBoth.vertexSet,
        withBoth.arcCount + 1L
      )

object Digraph:
  def empty[V: Hash]: Digraph[V] =
    new Digraph(
      HashMap.empty[V, HashSet[V]],
      HashMap.empty[V, HashSet[V]],
      VertexSet.empty[V],
      0L
    )

  def singleton[V: Hash](vertex: V): Digraph[V] =
    empty[V].addVertex(vertex)

  def of[V: Hash](
      vertices: IterableOnce[V],
      arcs: IterableOnce[ArcInput[V]]
  ): ValidatedNec[DigraphBuildError[V], Digraph[V]] =
    DigraphBuilder.authoritative(vertices, arcs)

  def fromArcs[V: Hash](
      arcs: IterableOnce[ArcInput[V]]
  ): ValidatedNec[DigraphBuildError[V], Digraph[V]] =
    DigraphBuilder.inferred(arcs)

  def fromArcs[V: Hash](
      arcs: ArcInput[V]*
  ): ValidatedNec[DigraphBuildError[V], Digraph[V]] =
    fromArcs(arcs: IterableOnce[ArcInput[V]])

  private[graph4s] def materialize[V: Hash](
      vertices: IterableOnce[V],
      arcs: IterableOnce[ArcInput[V]]
  ): Digraph[V] =
    DigraphBuilder.materialize(vertices, arcs)

  given [V: Show]: Show[Digraph[V]] =
    Show.show { graph =>
      val shownVertices =
        graph.vertices.iterator.map(Show[V].show).mkString("(", ", ", ")")
      val shownArcs =
        graph.arcs.iterator.map(Show[Arc[V]].show).mkString("(", ", ", ")")
      s"Digraph(vertices = $shownVertices, arcs = $shownArcs)"
    }

  given [V]: PartialOrder[Digraph[V]] with
    override def eqv(left: Digraph[V], right: Digraph[V]): Boolean =
      left.vertexCount == right.vertexCount &&
        left.arcCount == right.arcCount &&
        left.isSubgraphOf(right)

    def partialCompare(left: Digraph[V], right: Digraph[V]): Double =
      val leftBelow = left.isSubgraphOf(right)
      val rightBelow = right.isSubgraphOf(left)
      if leftBelow && rightBelow then 0.0
      else if leftBelow then -1.0
      else if rightBelow then 1.0
      else Double.NaN

  given [V: Hash]: BoundedSemilattice[Digraph[V]] with
    def empty: Digraph[V] = Digraph.empty[V]
    def combine(left: Digraph[V], right: Digraph[V]): Digraph[V] =
      left.overlay(right)

final case class InducedSubgraphDigraph[V](
    source: Digraph[V],
    graph: Digraph[V],
    omitted: VertexSet[V]
)

private object DigraphBuilder:
  private final class State[V](using V: Hash[V]):
    private val vertices = MutableHashSet[V]()
    private val outgoing = MutableHashMap[V, MutableHashSet[V]]()
    private val incoming = MutableHashMap[V, MutableHashSet[V]]()
    private var arcs = 0L

    def addVertex(vertex: V): Unit =
      if vertices.add(vertex) then
        outgoing.put(vertex, MutableHashSet[V]())
        incoming.put(vertex, MutableHashSet[V]())

    def containsVertex(vertex: V): Boolean =
      vertices.contains(vertex)

    def addArc(source: V, target: V): Unit =
      val successors = outgoing.getOrElseUpdate(source, MutableHashSet[V]())
      val predecessors = incoming.getOrElseUpdate(target, MutableHashSet[V]())
      if successors.add(target) then
        predecessors.add(source)
        arcs += 1L

    def freeze: Digraph[V] =
      val frozenOutgoing =
        HashMap.fromIterableOnce(
          outgoing.iterator.map { case (vertex, neighbors) =>
            (vertex, neighbors.freeze)
          }
        )
      val frozenIncoming =
        HashMap.fromIterableOnce(
          incoming.iterator.map { case (vertex, neighbors) =>
            (vertex, neighbors.freeze)
          }
        )
      new Digraph(
        frozenOutgoing,
        frozenIncoming,
        VertexSet.unsafe(vertices.freeze),
        arcs
      )

  def authoritative[V: Hash](
      suppliedVertices: IterableOnce[V],
      suppliedArcs: IterableOnce[ArcInput[V]]
  ): ValidatedNec[DigraphBuildError[V], Digraph[V]] =
    val V = Hash[V]
    val state = State[V]()
    suppliedVertices.iterator.foreach(state.addVertex)
    val errors = Vector.newBuilder[DigraphBuildError[V]]

    suppliedArcs.iterator.foreach { arc =>
      val loop = V.eqv(arc.source, arc.target)
      val sourceKnown = state.containsVertex(arc.source)
      val targetKnown = state.containsVertex(arc.target)
      if loop then errors += DigraphBuildError.SelfLoop(arc.source)
      if !sourceKnown then errors += DigraphBuildError.UnknownEndpoint(arc, arc.source)
      if !targetKnown && !V.eqv(arc.source, arc.target) then
        errors += DigraphBuildError.UnknownEndpoint(arc, arc.target)
      if !loop && sourceKnown && targetKnown then state.addArc(arc.source, arc.target)
    }

    cats.data.NonEmptyChain.fromSeq(errors.result()) match
      case Some(value) => value.invalid
      case None        => state.freeze.valid

  def inferred[V: Hash](
      suppliedArcs: IterableOnce[ArcInput[V]]
  ): ValidatedNec[DigraphBuildError[V], Digraph[V]] =
    val V = Hash[V]
    val state = State[V]()
    val errors = Vector.newBuilder[DigraphBuildError[V]]

    suppliedArcs.iterator.foreach { arc =>
      state.addVertex(arc.source)
      state.addVertex(arc.target)
      if V.eqv(arc.source, arc.target) then errors += DigraphBuildError.SelfLoop(arc.source)
      else state.addArc(arc.source, arc.target)
    }

    cats.data.NonEmptyChain.fromSeq(errors.result()) match
      case Some(value) => value.invalid
      case None        => state.freeze.valid

  def materialize[V: Hash](
      suppliedVertices: IterableOnce[V],
      suppliedArcs: IterableOnce[ArcInput[V]]
  ): Digraph[V] =
    val V = Hash[V]
    val state = State[V]()
    suppliedVertices.iterator.foreach(state.addVertex)
    suppliedArcs.iterator.foreach { arc =>
      if !V.eqv(arc.source, arc.target) then
        state.addVertex(arc.source)
        state.addVertex(arc.target)
        state.addArc(arc.source, arc.target)
    }
    state.freeze

  def overlay[V](left: Digraph[V], right: Digraph[V]): Digraph[V] =
    given Hash[V] = left.vertexHash
    val state = State[V]()
    left.vertices.iterator.foreach(state.addVertex)
    right.vertices.iterator.foreach(state.addVertex)
    left.arcs.iterator.foreach(arc => state.addArc(arc.source, arc.target))
    right.arcs.iterator.foreach(arc => state.addArc(arc.source, arc.target))
    state.freeze
