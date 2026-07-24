package graph4s.data

import cats.{Eq, Hash}
import cats.collections.HashMap
import cats.data.ValidatedNec
import cats.syntax.all.*
import graph4s.{Edge, Graph, InducedSubgraph}

enum EdgeDataError[V]:
  case UnknownEdge(edge: Edge[V])

final class EdgeMap[V, +A] private (
    val topology: Graph[V],
    private val values: HashMap[Edge[V], A]
):
  def get(edge: Edge[V]): Option[A] =
    if topology.containsEdge(edge.first, edge.second) then values.get(edge)
    else None

  def get(left: V, right: V): Option[A] =
    get(Edge.unsafe(left, right))

  def contains(edge: Edge[V]): Boolean =
    get(edge).nonEmpty

  def size: Long = values.size.toLong
  def iterator: Iterator[(Edge[V], A)] = values.iterator

  def mapValues[B](f: A => B): EdgeMap[V, B] =
    given Hash[V] = topology.vertexHash
    new EdgeMap(
      topology,
      HashMap.fromIterableOnce(
        values.iterator.map { case (edge, value) => (edge, f(value)) }
      )
    )

object EdgeMap:
  def empty[V, A](graph: Graph[V]): EdgeMap[V, A] =
    given Hash[V] = graph.vertexHash
    new EdgeMap(graph, HashMap.empty[Edge[V], A])

  def from[V, A](
      graph: Graph[V],
      entries: IterableOnce[(Edge[V], A)]
  ): ValidatedNec[EdgeDataError[V], EdgeMap[V, A]] =
    given Hash[V] = graph.vertexHash
    val materialized = entries.iterator.toVector
    val errors =
      materialized.iterator.collect {
        case (edge, _) if !graph.containsEdge(edge.first, edge.second) =>
          EdgeDataError.UnknownEdge(edge)
      }.toVector
    cats.data.NonEmptyChain.fromSeq(errors) match
      case Some(value) => value.invalid
      case None        =>
        new EdgeMap(
          graph,
          HashMap.fromIterableOnce(materialized)
        ).valid

final class EdgeField[V, +A] private (
    val topology: Graph[V],
    private val values: HashMap[Edge[V], A]
):
  def get(edge: Edge[V]): Option[A] =
    if topology.containsEdge(edge.first, edge.second) then values.get(edge)
    else None

  def get(left: V, right: V): Option[A] =
    get(Edge.unsafe(left, right))

  def iterator: Iterator[(Edge[V], A)] = values.iterator
  def size: Long = topology.edgeCount

  def map[B](f: A => B): EdgeField[V, B] =
    given Hash[V] = topology.vertexHash
    new EdgeField(
      topology,
      HashMap.fromIterableOnce(
        values.iterator.map { case (edge, value) => (edge, f(value)) }
      )
    )

object EdgeField:
  def total[V, A](graph: Graph[V])(value: Edge[V] => A): EdgeField[V, A] =
    given Hash[V] = graph.vertexHash
    new EdgeField(
      graph,
      HashMap.fromIterableOnce(
        graph.edges.iterator.map(edge => (edge, value(edge)))
      )
    )

  def restrict[V, A](
      induced: InducedSubgraph[V],
      field: EdgeField[V, A]
  ): Either[TopologyMismatch[V], EdgeField[V, A]] =
    if Eq[Graph[V]].eqv(induced.source, field.topology) then
      given Hash[V] = induced.graph.vertexHash
      Right(
        new EdgeField(
          induced.graph,
          HashMap.fromIterableOnce(
            induced.graph.edges.iterator.flatMap(edge => field.get(edge).map((edge, _)))
          )
        )
      )
    else Left(TopologyMismatch(induced.source, field.topology))
