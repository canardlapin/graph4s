package graph4s

import cats.{Eq, Hash, Show}
import scala.util.hashing.MurmurHash3

/** A validated edge of a simple undirected graph.
  *
  * Endpoint order is observational only: the Cats equality and hash instances treat `Edge(a, b)`
  * and `Edge(b, a)` as the same edge.
  */
final class Edge[V] private[graph4s] (
    val first: V,
    val second: V
):
  def contains(vertex: V)(using eqV: Eq[V]): Boolean =
    eqV.eqv(first, vertex) || eqV.eqv(second, vertex)

  def other(vertex: V)(using eqV: Eq[V]): Option[V] =
    if eqV.eqv(first, vertex) then Some(second)
    else if eqV.eqv(second, vertex) then Some(first)
    else None

  /** Universal equality follows ordinary Scala endpoint equality so public edge values remain
    * usable in `Set`, `Map`, `distinct`, and test assertions. Cats equality remains authoritative
    * when a custom `Hash[V]` is in scope.
    */
  override def equals(other: Any): Boolean =
    other match
      case edge: Edge[?] =>
        (first == edge.first && second == edge.second) ||
        (first == edge.second && second == edge.first)
      case _ =>
        false

  override def hashCode(): Int =
    MurmurHash3.unorderedHash(
      Iterator(first.##, second.##),
      MurmurHash3.productSeed
    )

  override def toString: String =
    s"Edge($first, $second)"

private[graph4s] trait EdgeLowPriorityInstances:
  given [V: Eq]: Eq[Edge[V]] =
    Eq.instance { (left, right) =>
      val V = Eq[V]
      (V.eqv(left.first, right.first) && V.eqv(left.second, right.second)) ||
      (V.eqv(left.first, right.second) && V.eqv(left.second, right.first))
    }

object Edge extends EdgeLowPriorityInstances:
  private[graph4s] def unsafe[V](first: V, second: V): Edge[V] =
    new Edge(first, second)

  given [V: Hash]: Hash[Edge[V]] with
    def eqv(left: Edge[V], right: Edge[V]): Boolean =
      val V = Hash[V]
      (V.eqv(left.first, right.first) && V.eqv(left.second, right.second)) ||
      (V.eqv(left.first, right.second) && V.eqv(left.second, right.first))

    def hash(edge: Edge[V]): Int =
      val V = Hash[V]
      MurmurHash3.unorderedHash(
        Iterator(V.hash(edge.first), V.hash(edge.second)),
        MurmurHash3.productSeed
      )

  given [V: Show]: Show[Edge[V]] =
    Show.show(edge => s"${Show[V].show(edge.first)} -- ${Show[V].show(edge.second)}")

/** A validated arc of a simple directed graph.
  *
  * Universal value semantics use ordinary endpoint equality; the Cats `Hash` instance uses the
  * graph's coherent `Hash[V]`.
  */
final class Arc[V] private[graph4s] (
    val source: V,
    val target: V
):
  override def equals(other: Any): Boolean =
    other match
      case arc: Arc[?] =>
        source == arc.source && target == arc.target
      case _ =>
        false

  override def hashCode(): Int =
    MurmurHash3.mixLast(MurmurHash3.mix(MurmurHash3.productSeed, source.##), target.##)

  override def toString: String =
    s"Arc($source, $target)"

object Arc:
  private[graph4s] def unsafe[V](source: V, target: V): Arc[V] =
    new Arc(source, target)

  given [V: Hash]: Hash[Arc[V]] with
    def eqv(left: Arc[V], right: Arc[V]): Boolean =
      val V = Hash[V]
      V.eqv(left.source, right.source) && V.eqv(left.target, right.target)

    def hash(arc: Arc[V]): Int =
      val V = Hash[V]
      31 * V.hash(arc.source) + V.hash(arc.target)

  given [V: Show]: Show[Arc[V]] =
    Show.show(arc => s"${Show[V].show(arc.source)} --> ${Show[V].show(arc.target)}")

/** Unvalidated undirected construction input. */
final case class Link[V](left: V, right: V)

/** Unvalidated directed construction input. */
final case class ArcInput[V](source: V, target: V)
