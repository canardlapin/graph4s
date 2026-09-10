package graph4s.data

import cats.Eq
import graph4s.{Digraph, Graph}
import graph4s.indexed.{IndexedDigraph, IndexedGraph}

/** A total edge field materialized in one indexed graph's edge order. */
final class IndexedEdgeField[V, +A, I <: IndexedGraph[V]] private (
    val index: I,
    private val values: Vector[A]
):
  require(values.length == index.edgeCount, "indexed edge data must match the indexed topology")

  def size: Int = values.length

  def apply(edge: index.Edge): A =
    values(index.edgeOrdinal(edge))

  def valueAt(ordinal: Int): Option[A] =
    values.lift(ordinal)

  private[graph4s] def unsafeValueAt(ordinal: Int): A =
    values(ordinal)

  def iterator: Iterator[A] =
    values.iterator

  def map[B](f: A => B): IndexedEdgeField[V, B, I] =
    new IndexedEdgeField[V, B, I](index, values.map(f))

object IndexedEdgeField:
  def from[V, A](
      index: IndexedGraph[V],
      field: EdgeField[V, A]
  ): Either[TopologyMismatch[V], IndexedEdgeField[V, A, index.type]] =
    if Eq[Graph[V]].eqv(index.toGraph, field.topology) then
      Right(
        new IndexedEdgeField[V, A, index.type](
          index,
          index.edges.map { edge =>
            field.get(index.label(index.first(edge)), index.label(index.second(edge))).get
          }.toVector
        )
      )
    else Left(TopologyMismatch(index.toGraph, field.topology))

/** A total arc field materialized in one indexed digraph's arc order. */
final class IndexedArcField[V, +A, I <: IndexedDigraph[V]] private (
    val index: I,
    private val values: Vector[A]
):
  require(values.length == index.arcCount, "indexed arc data must match the indexed topology")

  def size: Int = values.length

  def apply(arc: index.Arc): A =
    values(index.arcOrdinal(arc))

  def valueAt(ordinal: Int): Option[A] =
    values.lift(ordinal)

  private[graph4s] def unsafeValueAt(ordinal: Int): A =
    values(ordinal)

  def iterator: Iterator[A] =
    values.iterator

  def map[B](f: A => B): IndexedArcField[V, B, I] =
    new IndexedArcField[V, B, I](index, values.map(f))

object IndexedArcField:
  def from[V, A](
      index: IndexedDigraph[V],
      field: ArcField[V, A]
  ): Either[DigraphTopologyMismatch[V], IndexedArcField[V, A, index.type]] =
    if Eq[Digraph[V]].eqv(index.toDigraph, field.topology) then
      Right(
        new IndexedArcField[V, A, index.type](
          index,
          index.arcs.map { arc =>
            field.get(index.label(index.source(arc)), index.label(index.target(arc))).get
          }.toVector
        )
      )
    else Left(DigraphTopologyMismatch(index.toDigraph, field.topology))
