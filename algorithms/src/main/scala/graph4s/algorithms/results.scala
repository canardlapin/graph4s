package graph4s.algorithms

import cats.data.NonEmptyVector
import graph4s.data.VertexMap
import graph4s.{Digraph, Graph}

/** A nonempty vertex path. `length` counts edges, not vertices. */
final class Path[V] private (val vertices: NonEmptyVector[V]):
  def length: Int = vertices.length - 1
  def start: V = vertices.head
  def end: V = vertices.last

  override def equals(other: Any): Boolean =
    other match
      case path: Path[?] => vertices == path.vertices
      case _             => false

  override def hashCode(): Int =
    vertices.hashCode()

  override def toString: String =
    vertices.toVector.mkString("Path(", ", ", ")")

object Path:
  def fromVertices[V](vertices: Vector[V]): Option[Path[V]] =
    NonEmptyVector.fromVector(vertices).map(new Path(_))

  def one[V](vertex: V): Path[V] =
    new Path(NonEmptyVector.one(vertex))

final case class BfsResult[V](
    root: V,
    order: Vector[V],
    distance: VertexMap[V, Int],
    parent: VertexMap[V, Option[V]]
):
  def pathTo(vertex: V): Option[Path[V]] =
    if !distance.contains(vertex) then None
    else
      val reversed = Vector.newBuilder[V]
      var current = vertex
      var continue = true
      while continue do
        reversed += current
        parent.get(current) match
          case Some(Some(previous)) =>
            current = previous
          case Some(None) =>
            continue = false
          case None =>
            continue = false
      Path.fromVertices(reversed.result().reverse)

final case class DfsResult[V](
    root: V,
    preorder: Vector[V],
    parent: VertexMap[V, Option[V]]
)

final case class Components[V](
    components: Vector[graph4s.VertexSet[V]],
    componentOf: VertexMap[V, Int]
):
  def componentContaining(vertex: V): Option[graph4s.VertexSet[V]] =
    componentOf.get(vertex).flatMap(components.lift)

final case class SpanningTree[V](
    root: V,
    parent: VertexMap[V, Option[V]]
)

final class ConnectedGraph[V] private[algorithms] (
    val graph: Graph[V],
    val spanningTree: SpanningTree[V]
)

final case class DirectedCycle[V](vertices: NonEmptyVector[V])

/** Evidence that a directed graph is acyclic, retaining both computations produced by validation.
  */
final class Dag[V] private[algorithms] (
    val graph: Digraph[V],
    val topologicalOrder: Vector[V],
    val layers: Vector[NonEmptyVector[V]]
)
