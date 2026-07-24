package graph4s

import cats.{Hash, Show}
import cats.collections.HashSet
import scala.util.hashing.MurmurHash3

/** A finite set whose membership uses a Cats [[Hash]] instance.
  *
  * Iteration order is intentionally unspecified. Ask for an [[cats.Order]] before turning a vertex
  * set into a reproducibly ordered value.
  */
final class VertexSet[V] private[graph4s] (
    private[graph4s] val values: HashSet[V]
):
  def contains(vertex: V): Boolean = values.contains(vertex)
  def iterator: Iterator[V] = values.iterator
  def size: Int = values.size
  def isEmpty: Boolean = values.isEmpty
  def nonEmpty: Boolean = values.nonEmpty
  def toVector: Vector[V] = iterator.toVector

  def sorted(using order: cats.Order[V]): Vector[V] =
    iterator.toVector.sorted(using order.toOrdering)

  def filter(predicate: V => Boolean): VertexSet[V] =
    VertexSet.unsafe(values.filter(predicate))

  def union(other: VertexSet[V]): VertexSet[V] =
    VertexSet.unsafe(values.union(other.values))

  def intersect(other: VertexSet[V]): VertexSet[V] =
    VertexSet.unsafe(values.intersect(other.values))

  def diff(other: VertexSet[V]): VertexSet[V] =
    VertexSet.unsafe(values.diff(other.values))

  private[graph4s] def add(vertex: V): VertexSet[V] =
    VertexSet.unsafe(values.add(vertex))

  private[graph4s] def remove(vertex: V): VertexSet[V] =
    VertexSet.unsafe(values.remove(vertex))

  override def equals(other: Any): Boolean =
    other match
      case set: VertexSet[?] =>
        size == set.size &&
        iterator.forall(value => set.iterator.exists(_ == value))
      case _ =>
        false

  override def hashCode(): Int =
    MurmurHash3.unorderedHash(iterator.map(_.##), MurmurHash3.setSeed)

  override def toString: String =
    iterator.mkString("VertexSet(", ", ", ")")

object VertexSet:
  def empty[V: Hash]: VertexSet[V] =
    unsafe(HashSet.empty[V])

  def from[V: Hash](vertices: IterableOnce[V]): VertexSet[V] =
    unsafe(HashSet.fromIterableOnce(vertices))

  private[graph4s] def unsafe[V](values: HashSet[V]): VertexSet[V] =
    new VertexSet(values)

  given [V: Hash]: Hash[VertexSet[V]] with
    def eqv(left: VertexSet[V], right: VertexSet[V]): Boolean =
      left.values === right.values

    def hash(set: VertexSet[V]): Int =
      MurmurHash3.unorderedHash(
        set.iterator.map(Hash[V].hash),
        MurmurHash3.setSeed
      )

  given [V: Show]: Show[VertexSet[V]] =
    Show.show(set => set.iterator.map(Show[V].show).mkString("VertexSet(", ", ", ")"))
