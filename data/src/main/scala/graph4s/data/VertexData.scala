package graph4s.data

import cats.Eq
import cats.collections.HashMap
import cats.data.ValidatedNec
import cats.syntax.all.*
import graph4s.{Graph, InducedSubgraph, VertexDomain}

enum VertexDataError[V]:
  case UnknownVertex(vertex: V)

final class VertexMap[V, +A] private[graph4s] (
    val topology: VertexDomain[V],
    private val values: HashMap[V, A]
):
  private lazy val universalValues: Map[Any, Any] =
    values.iterator.map { case (vertex, value) =>
      (vertex: Any) -> (value: Any)
    }.toMap

  def get(vertex: V): Option[A] =
    if topology.containsVertex(vertex) then values.get(vertex) else None

  def contains(vertex: V): Boolean =
    topology.containsVertex(vertex) && values.contains(vertex)

  def size: Int = values.size
  def iterator: Iterator[(V, A)] = values.iterator

  def mapValues[B](f: A => B): VertexMap[V, B] =
    new VertexMap(
      topology,
      HashMap.fromIterableOnce(
        values.iterator.map { case (vertex, value) => (vertex, f(value)) }
      )(using topology.vertexHash)
    )

  override def equals(other: Any): Boolean =
    other match
      case map: VertexMap[?, ?] =>
        size == map.size &&
        topology.vertices == map.topology.vertices &&
        universalValues == map.universalValues
      case _ =>
        false

  override def hashCode(): Int =
    31 * topology.vertices.hashCode() + universalValues.hashCode()

  override def toString: String =
    universalValues.mkString("VertexMap(", ", ", ")")

object VertexMap:
  private[graph4s] def fromTrusted[V, A](
      graph: VertexDomain[V],
      entries: IterableOnce[(V, A)]
  ): VertexMap[V, A] =
    new VertexMap(
      graph,
      HashMap.fromIterableOnce(entries)(using graph.vertexHash)
    )

  def empty[V, A](graph: VertexDomain[V]): VertexMap[V, A] =
    new VertexMap(
      graph,
      HashMap.empty[V, A](using graph.vertexHash)
    )

  def from[V, A](
      graph: VertexDomain[V],
      entries: IterableOnce[(V, A)]
  ): ValidatedNec[VertexDataError[V], VertexMap[V, A]] =
    val materialized = entries.iterator.toVector
    val errors =
      materialized.iterator.collect {
        case (vertex, _) if !graph.containsVertex(vertex) =>
          VertexDataError.UnknownVertex(vertex)
      }.toVector
    cats.data.NonEmptyChain.fromSeq(errors) match
      case Some(value) => value.invalid
      case None        =>
        new VertexMap(
          graph,
          HashMap.fromIterableOnce(materialized)(using graph.vertexHash)
        ).valid

final class VertexField[V, +A] private (
    val topology: Graph[V],
    private val values: HashMap[V, A]
):
  def get(vertex: V): Option[A] =
    if topology.containsVertex(vertex) then values.get(vertex) else None

  def iterator: Iterator[(V, A)] = values.iterator
  def size: Int = topology.vertexCount

  def map[B](f: A => B): VertexField[V, B] =
    new VertexField(
      topology,
      HashMap.fromIterableOnce(
        values.iterator.map { case (vertex, value) => (vertex, f(value)) }
      )(using topology.vertexHash)
    )

object VertexField:
  def total[V, A](graph: Graph[V])(value: V => A): VertexField[V, A] =
    new VertexField(
      graph,
      HashMap.fromIterableOnce(
        graph.vertices.iterator.map(vertex => (vertex, value(vertex)))
      )(using graph.vertexHash)
    )

  def restrict[V, A](
      induced: InducedSubgraph[V],
      field: VertexField[V, A]
  ): Either[TopologyMismatch[V], VertexField[V, A]] =
    if Eq[Graph[V]].eqv(induced.source, field.topology) then
      Right(
        new VertexField(
          induced.graph,
          HashMap.fromIterableOnce(
            induced.graph.vertices.iterator.flatMap(vertex => field.get(vertex).map((vertex, _)))
          )(using induced.graph.vertexHash)
        )
      )
    else Left(TopologyMismatch(induced.source, field.topology))
