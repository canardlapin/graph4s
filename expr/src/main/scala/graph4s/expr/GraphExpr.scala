package graph4s.expr

import cats.{Functor, Hash}
import cats.collections.HashSet
import graph4s.{Graph, Link}
import scala.annotation.tailrec
import scala.util.hashing.MurmurHash3

/** A compositional description of a finite simple undirected graph.
  *
  * `Join(left, right)` overlays both operands and connects every pair of distinct labels across the
  * operands. Expressions may contain duplicate labels and loops syntactically; compilation applies
  * simple-graph set semantics and removes loops.
  */
enum GraphExpr[+V]:
  case Empty
  case Vertex(value: V)
  case Overlay(left: GraphExpr[V], right: GraphExpr[V])
  case Join(left: GraphExpr[V], right: GraphExpr[V])

  infix def overlay[W >: V](other: GraphExpr[W]): GraphExpr[W] =
    GraphExpr.Overlay(this, other)

  infix def join[W >: V](other: GraphExpr[W]): GraphExpr[W] =
    GraphExpr.Join(this, other)

  def compile[W >: V](using Hash[W]): Graph[W] =
    GraphExpr.compile(this)

object GraphExpr:
  def empty[V]: GraphExpr[V] = Empty
  def vertex[V](value: V): GraphExpr[V] = Vertex(value)

  def vertices[V](values: V*): GraphExpr[V] =
    balancedOverlay(values.iterator.map(Vertex(_)).toVector)

  def path[V](values: IterableOnce[V]): GraphExpr[V] =
    val labels = values.iterator.toVector
    if labels.isEmpty then Empty
    else
      val edges =
        labels.iterator
          .zip(labels.iterator.drop(1))
          .map { case (left, right) =>
            Join(Vertex(left), Vertex(right))
          }
          .toVector
      balancedOverlay(Vertex(labels.head) +: edges)

  /** Closes a path. With fewer than three distinct labels, simple-graph semantics degenerate to a
    * vertex or a single edge.
    */
  def cycle[V](values: IterableOnce[V]): GraphExpr[V] =
    val labels = values.iterator.toVector
    if labels.size <= 1 then path(labels)
    else path(labels).overlay(Join(Vertex(labels.last), Vertex(labels.head)))

  def clique[V](values: IterableOnce[V]): GraphExpr[V] =
    val labels = values.iterator.toVector
    if labels.isEmpty then Empty
    else Join(vertices(labels*), vertices(labels*))

  def star[V](center: V, leaves: IterableOnce[V]): GraphExpr[V] =
    Join(Vertex(center), balancedOverlay(leaves.iterator.map(Vertex(_)).toVector))

  def biclique[V](
      left: IterableOnce[V],
      right: IterableOnce[V]
  ): GraphExpr[V] =
    Join(
      balancedOverlay(left.iterator.map(Vertex(_)).toVector),
      balancedOverlay(right.iterator.map(Vertex(_)).toVector)
    )

  def compile[V: Hash](expression: GraphExpr[V]): Graph[V] =
    val fragment = Compiler.run(expression)
    Graph.materialize(
      fragment.vertices.iterator,
      fragment.edges.iterator.map(edge => Link(edge.left, edge.right))
    )

  given Functor[GraphExpr] with
    def map[A, B](source: GraphExpr[A])(f: A => B): GraphExpr[B] =
      mapStackSafe(source)(f)

  private def mapStackSafe[A, B](
      root: GraphExpr[A]
  )(f: A => B): GraphExpr[B] =
    foldStackSafe(root)(
      empty = Empty,
      vertex = value => Vertex(f(value)),
      overlay = Overlay(_, _),
      join = Join(_, _)
    )

  private def balancedOverlay[V](
      expressions: Vector[GraphExpr[V]]
  ): GraphExpr[V] =
    if expressions.isEmpty then Empty
    else
      var level = expressions
      while level.size > 1 do
        val next = Vector.newBuilder[GraphExpr[V]]
        var index = 0
        while index < level.size do
          if index + 1 < level.size then next += Overlay(level(index), level(index + 1))
          else next += level(index)
          index += 2
        level = next.result()
      level.head

  private enum BinaryOperation:
    case Overlay
    case Join

  private enum Context[+A, +R]:
    case Done
    case EvaluateRight(
        right: GraphExpr[A],
        operation: BinaryOperation,
        parent: Context[A, R]
    )
    case Rebuild(
        left: R,
        operation: BinaryOperation,
        parent: Context[A, R]
    )

  private enum Evaluation[A, R]:
    case Evaluate(expression: GraphExpr[A], context: Context[A, R])
    case Return(value: R, context: Context[A, R])

  /** A zipper interpreter. Each rebuild frame owns its completed left value, so an operand stack
    * cannot underflow.
    */
  private def foldStackSafe[A, R](
      root: GraphExpr[A]
  )(
      empty: => R,
      vertex: A => R,
      overlay: (R, R) => R,
      join: (R, R) => R
  ): R =
    @tailrec
    def loop(state: Evaluation[A, R]): R =
      state match
        case Evaluation.Evaluate(expression, context) =>
          expression match
            case Empty =>
              loop(Evaluation.Return(empty, context))
            case Vertex(value) =>
              loop(Evaluation.Return(vertex(value), context))
            case Overlay(left, right) =>
              loop(
                Evaluation.Evaluate(
                  left,
                  Context.EvaluateRight(
                    right,
                    BinaryOperation.Overlay,
                    context
                  )
                )
              )
            case Join(left, right) =>
              loop(
                Evaluation.Evaluate(
                  left,
                  Context.EvaluateRight(
                    right,
                    BinaryOperation.Join,
                    context
                  )
                )
              )
        case Evaluation.Return(value, context) =>
          context match
            case Context.Done =>
              value
            case Context.EvaluateRight(right, operation, parent) =>
              loop(
                Evaluation.Evaluate(
                  right,
                  Context.Rebuild(value, operation, parent)
                )
              )
            case Context.Rebuild(left, operation, parent) =>
              val rebuilt =
                operation match
                  case BinaryOperation.Overlay => overlay(left, value)
                  case BinaryOperation.Join    => join(left, value)
              loop(Evaluation.Return(rebuilt, parent))

    loop(Evaluation.Evaluate(root, Context.Done))

  private final case class UnorderedEdge[V](left: V, right: V)

  private object UnorderedEdge:
    given [V: Hash]: Hash[UnorderedEdge[V]] with
      def eqv(left: UnorderedEdge[V], right: UnorderedEdge[V]): Boolean =
        val V = Hash[V]
        (V.eqv(left.left, right.left) && V.eqv(left.right, right.right)) ||
        (V.eqv(left.left, right.right) && V.eqv(left.right, right.left))

      def hash(edge: UnorderedEdge[V]): Int =
        val V = Hash[V]
        MurmurHash3.unorderedHash(
          Iterator(V.hash(edge.left), V.hash(edge.right)),
          MurmurHash3.productSeed
        )

  private final case class Fragment[V](
      vertices: HashSet[V],
      edges: HashSet[UnorderedEdge[V]]
  )

  private object Compiler:
    def run[V: Hash](root: GraphExpr[V]): Fragment[V] =
      val V = Hash[V]
      def emptyFragment: Fragment[V] =
        Fragment(
          HashSet.empty[V],
          HashSet.empty[UnorderedEdge[V]]
        )

      def overlay(
          left: Fragment[V],
          right: Fragment[V]
      ): Fragment[V] =
        Fragment(
          left.vertices.union(right.vertices),
          left.edges.union(right.edges)
        )

      foldStackSafe(root)(
        empty = emptyFragment,
        vertex = value =>
          Fragment(
            HashSet(value),
            HashSet.empty[UnorderedEdge[V]]
          ),
        overlay = overlay,
        join = (left, right) =>
          val base = overlay(left, right)
          val cross =
            left.vertices.iterator.flatMap(a =>
              right.vertices.iterator
                .filterNot(V.eqv(a, _))
                .map(b => UnorderedEdge(a, b))
            )
          base.copy(edges = base.edges.union(cross))
      )
