package graph4s.indexed

import cats.Order

/** A reproducible policy for assigning graph-scoped integer vertices. */
enum VertexOrder[V]:
  case By(order: Order[V])
  case Explicit(vertices: Vector[V])

object VertexOrder:
  def by[V](order: Order[V]): VertexOrder[V] =
    VertexOrder.By(order)

  def explicit[V](vertices: Vector[V]): VertexOrder[V] =
    VertexOrder.Explicit(vertices)

enum IndexingError[V]:
  case DuplicateVertex(vertex: V)
  case UnknownVertex(vertex: V)
  case MissingVertex(vertex: V)
  case InconsistentTopology(vertex: V)
  case AdjacencyTooLarge(entries: Long)

private[indexed] object IndexingLimits:
  def arrayLength[V](entries: Long): Either[IndexingError[V], Int] =
    if entries > Int.MaxValue.toLong then Left(IndexingError.AdjacencyTooLarge(entries))
    else Right(entries.toInt)
