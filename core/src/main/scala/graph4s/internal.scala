package graph4s

import cats.Hash
import cats.collections.HashSet
import scala.collection.mutable

private[graph4s] final class MutableHashSet[A](using A: Hash[A]):
  private val buckets = mutable.HashMap.empty[Int, mutable.ArrayBuffer[A]]
  private var count = 0

  def size: Int = count

  def contains(value: A): Boolean =
    buckets.get(A.hash(value)).exists(_.exists(A.eqv(_, value)))

  /** Returns true exactly when the value was newly inserted. */
  def add(value: A): Boolean =
    val bucket = buckets.getOrElseUpdate(A.hash(value), mutable.ArrayBuffer.empty)
    if bucket.exists(A.eqv(_, value)) then false
    else
      bucket += value
      count += 1
      true

  def iterator: Iterator[A] =
    buckets.valuesIterator.flatMap(_.iterator)

  def freeze: HashSet[A] =
    HashSet.fromIterableOnce(iterator)

private[graph4s] final class MutableHashMap[K, V](using K: Hash[K]):
  private val buckets =
    mutable.HashMap.empty[Int, mutable.ArrayBuffer[(K, V)]]

  def get(key: K): Option[V] =
    buckets
      .get(K.hash(key))
      .flatMap(_.find(entry => K.eqv(entry._1, key)).map(_._2))

  def put(key: K, value: V): Unit =
    val bucket = buckets.getOrElseUpdate(K.hash(key), mutable.ArrayBuffer.empty)
    val index = bucket.indexWhere(entry => K.eqv(entry._1, key))
    if index < 0 then bucket += ((key, value))
    else bucket(index) = ((key, value))

  def getOrElseUpdate(key: K, value: => V): V =
    get(key) match
      case Some(existing) => existing
      case None           =>
        val created = value
        put(key, created)
        created

  def iterator: Iterator[(K, V)] =
    buckets.valuesIterator.flatMap(_.iterator)

private[graph4s] object InternalHash:
  def either[A, B](left: Hash[A], right: Hash[B]): Hash[Either[A, B]] =
    new Hash[Either[A, B]]:
      def eqv(x: Either[A, B], y: Either[A, B]): Boolean =
        (x, y) match
          case (Left(a), Left(b))   => left.eqv(a, b)
          case (Right(a), Right(b)) => right.eqv(a, b)
          case _                    => false

      def hash(value: Either[A, B]): Int =
        value match
          case Left(a)  => 31 * left.hash(a) + 1
          case Right(b) => 31 * right.hash(b) + 2

  def tuple2[A, B](left: Hash[A], right: Hash[B]): Hash[(A, B)] =
    new Hash[(A, B)]:
      def eqv(x: (A, B), y: (A, B)): Boolean =
        left.eqv(x._1, y._1) && right.eqv(x._2, y._2)

      def hash(value: (A, B)): Int =
        31 * left.hash(value._1) + right.hash(value._2)
