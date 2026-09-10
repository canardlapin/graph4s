package graph4s.data

import cats.{Eq, Hash}
import cats.collections.HashMap
import cats.data.ValidatedNec
import cats.syntax.all.*
import graph4s.{Arc, Digraph, InducedSubgraphDigraph}

enum ArcDataError[V]:
  case UnknownArc(arc: Arc[V])

final class ArcMap[V, +A] private (
    val topology: Digraph[V],
    private val values: HashMap[Arc[V], A]
):
  def get(arc: Arc[V]): Option[A] =
    if topology.containsArc(arc.source, arc.target) then values.get(arc)
    else None

  def get(source: V, target: V): Option[A] =
    get(Arc.unsafe(source, target))

  def contains(arc: Arc[V]): Boolean =
    get(arc).nonEmpty

  def size: Long = values.size.toLong
  def iterator: Iterator[(Arc[V], A)] = values.iterator

  def mapValues[B](f: A => B): ArcMap[V, B] =
    given Hash[V] = topology.vertexHash
    new ArcMap(
      topology,
      HashMap.fromIterableOnce(
        values.iterator.map { case (arc, value) => (arc, f(value)) }
      )
    )

object ArcMap:
  def empty[V, A](graph: Digraph[V]): ArcMap[V, A] =
    given Hash[V] = graph.vertexHash
    new ArcMap(graph, HashMap.empty[Arc[V], A])

  def from[V, A](
      graph: Digraph[V],
      entries: IterableOnce[(Arc[V], A)]
  ): ValidatedNec[ArcDataError[V], ArcMap[V, A]] =
    given Hash[V] = graph.vertexHash
    val materialized = entries.iterator.toVector
    val errors =
      materialized.iterator.collect {
        case (arc, _) if !graph.containsArc(arc.source, arc.target) =>
          ArcDataError.UnknownArc(arc)
      }.toVector
    cats.data.NonEmptyChain.fromSeq(errors) match
      case Some(value) => value.invalid
      case None        =>
        new ArcMap(
          graph,
          HashMap.fromIterableOnce(materialized)
        ).valid

final class ArcField[V, +A] private (
    val topology: Digraph[V],
    private val values: HashMap[Arc[V], A]
):
  def get(arc: Arc[V]): Option[A] =
    if topology.containsArc(arc.source, arc.target) then values.get(arc)
    else None

  def get(source: V, target: V): Option[A] =
    get(Arc.unsafe(source, target))

  def iterator: Iterator[(Arc[V], A)] = values.iterator
  def size: Long = topology.arcCount

  def map[B](f: A => B): ArcField[V, B] =
    given Hash[V] = topology.vertexHash
    new ArcField(
      topology,
      HashMap.fromIterableOnce(
        values.iterator.map { case (arc, value) => (arc, f(value)) }
      )
    )

object ArcField:
  def total[V, A](graph: Digraph[V])(value: Arc[V] => A): ArcField[V, A] =
    given Hash[V] = graph.vertexHash
    new ArcField(
      graph,
      HashMap.fromIterableOnce(
        graph.arcs.iterator.map(arc => (arc, value(arc)))
      )
    )

  def restrict[V, A](
      induced: InducedSubgraphDigraph[V],
      field: ArcField[V, A]
  ): Either[DigraphTopologyMismatch[V], ArcField[V, A]] =
    if Eq[Digraph[V]].eqv(induced.source, field.topology) then
      given Hash[V] = induced.graph.vertexHash
      Right(
        new ArcField(
          induced.graph,
          HashMap.fromIterableOnce(
            induced.graph.arcs.iterator.flatMap(arc => field.get(arc).map((arc, _)))
          )
        )
      )
    else Left(DigraphTopologyMismatch(induced.source, field.topology))
