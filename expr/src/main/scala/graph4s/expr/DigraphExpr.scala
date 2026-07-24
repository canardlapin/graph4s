package graph4s.expr

import cats.{Functor, Hash}
import cats.collections.HashSet
import graph4s.{ArcInput, Digraph}
import scala.annotation.tailrec

/** A compositional description of a finite simple directed graph.
  *
  * `Connect(from, to)` overlays both operands and adds every non-loop arc from a label in `from` to
  * a label in `to`.
  */
enum DigraphExpr[+V]:
  case Empty
  case Vertex(value: V)
  case Overlay(left: DigraphExpr[V], right: DigraphExpr[V])
  case Connect(from: DigraphExpr[V], to: DigraphExpr[V])

  infix def overlay[W >: V](other: DigraphExpr[W]): DigraphExpr[W] =
    DigraphExpr.Overlay(this, other)

  infix def connect[W >: V](other: DigraphExpr[W]): DigraphExpr[W] =
    DigraphExpr.Connect(this, other)

  def compile[W >: V](using Hash[W]): Digraph[W] =
    DigraphExpr.compile(this)

object DigraphExpr:
  def empty[V]: DigraphExpr[V] = Empty
  def vertex[V](value: V): DigraphExpr[V] = Vertex(value)

  def vertices[V](values: V*): DigraphExpr[V] =
    balancedOverlay(values.iterator.map(Vertex(_)).toVector)

  def path[V](values: IterableOnce[V]): DigraphExpr[V] =
    val labels = values.iterator.toVector
    if labels.isEmpty then Empty
    else
      val arcs =
        labels.iterator
          .zip(labels.iterator.drop(1))
          .map { case (from, to) =>
            Connect(Vertex(from), Vertex(to))
          }
          .toVector
      balancedOverlay(Vertex(labels.head) +: arcs)

  /** Closes a directed path. A singleton remains loopless. */
  def cycle[V](values: IterableOnce[V]): DigraphExpr[V] =
    val labels = values.iterator.toVector
    if labels.size <= 1 then path(labels)
    else path(labels).overlay(Connect(Vertex(labels.last), Vertex(labels.head)))

  /** The loopless complete digraph, containing both orientations for every pair of distinct labels.
    */
  def complete[V](values: IterableOnce[V]): DigraphExpr[V] =
    val all = balancedOverlay(values.iterator.map(Vertex(_)).toVector)
    Connect(all, all)

  def compile[V: Hash](expression: DigraphExpr[V]): Digraph[V] =
    val fragment = Compiler.run(expression)
    Digraph.materialize(
      fragment.vertices.iterator,
      fragment.arcs.iterator.map(arc => ArcInput(arc.source, arc.target))
    )

  given Functor[DigraphExpr] with
    def map[A, B](source: DigraphExpr[A])(f: A => B): DigraphExpr[B] =
      mapStackSafe(source)(f)

  private def mapStackSafe[A, B](
      root: DigraphExpr[A]
  )(f: A => B): DigraphExpr[B] =
    foldStackSafe(root)(
      empty = Empty,
      vertex = value => Vertex(f(value)),
      overlay = Overlay(_, _),
      connect = Connect(_, _)
    )

  private def balancedOverlay[V](
      expressions: Vector[DigraphExpr[V]]
  ): DigraphExpr[V] =
    if expressions.isEmpty then Empty
    else
      var level = expressions
      while level.size > 1 do
        val next = Vector.newBuilder[DigraphExpr[V]]
        var index = 0
        while index < level.size do
          if index + 1 < level.size then next += Overlay(level(index), level(index + 1))
          else next += level(index)
          index += 2
        level = next.result()
      level.head

  private enum BinaryOperation:
    case Overlay
    case Connect

  private enum Context[+A, +R]:
    case Done
    case EvaluateRight(
        right: DigraphExpr[A],
        operation: BinaryOperation,
        parent: Context[A, R]
    )
    case Rebuild(
        left: R,
        operation: BinaryOperation,
        parent: Context[A, R]
    )

  private enum Evaluation[A, R]:
    case Evaluate(expression: DigraphExpr[A], context: Context[A, R])
    case Return(value: R, context: Context[A, R])

  private def foldStackSafe[A, R](
      root: DigraphExpr[A]
  )(
      empty: => R,
      vertex: A => R,
      overlay: (R, R) => R,
      connect: (R, R) => R
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
            case Connect(from, to) =>
              loop(
                Evaluation.Evaluate(
                  from,
                  Context.EvaluateRight(
                    to,
                    BinaryOperation.Connect,
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
                  case BinaryOperation.Connect => connect(left, value)
              loop(Evaluation.Return(rebuilt, parent))

    loop(Evaluation.Evaluate(root, Context.Done))

  private final case class DirectedArc[V](source: V, target: V)

  private object DirectedArc:
    given [V: Hash]: Hash[DirectedArc[V]] with
      def eqv(left: DirectedArc[V], right: DirectedArc[V]): Boolean =
        val V = Hash[V]
        V.eqv(left.source, right.source) && V.eqv(left.target, right.target)

      def hash(arc: DirectedArc[V]): Int =
        val V = Hash[V]
        31 * V.hash(arc.source) + V.hash(arc.target)

  private final case class Fragment[V](
      vertices: HashSet[V],
      arcs: HashSet[DirectedArc[V]]
  )

  private object Compiler:
    def run[V: Hash](root: DigraphExpr[V]): Fragment[V] =
      val V = Hash[V]
      def emptyFragment: Fragment[V] =
        Fragment(
          HashSet.empty[V],
          HashSet.empty[DirectedArc[V]]
        )

      def overlay(
          left: Fragment[V],
          right: Fragment[V]
      ): Fragment[V] =
        Fragment(
          left.vertices.union(right.vertices),
          left.arcs.union(right.arcs)
        )

      foldStackSafe(root)(
        empty = emptyFragment,
        vertex = value =>
          Fragment(
            HashSet(value),
            HashSet.empty[DirectedArc[V]]
          ),
        overlay = overlay,
        connect = (left, right) =>
          val base = overlay(left, right)
          val cross =
            left.vertices.iterator.flatMap(source =>
              right.vertices.iterator
                .filterNot(V.eqv(source, _))
                .map(target => DirectedArc(source, target))
            )
          base.copy(arcs = base.arcs.union(cross))
      )
