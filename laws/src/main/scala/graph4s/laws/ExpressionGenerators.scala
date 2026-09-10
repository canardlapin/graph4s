package graph4s.laws

import cats.Eq
import graph4s.expr.{DigraphExpr, GraphExpr}
import org.scalacheck.{Arbitrary, Gen}

object ExpressionGenerators:
  def graphExpr[A](using Arbitrary[A]): Gen[GraphExpr[A]] =
    Gen.sized { size =>
      if size <= 0 then
        Gen.oneOf(
          Gen.const(GraphExpr.Empty),
          Arbitrary.arbitrary[A].map(GraphExpr.Vertex(_))
        )
      else
        val child = Gen.resize(size / 2, graphExpr[A])
        Gen.frequency(
          1 -> Gen.const(GraphExpr.Empty),
          3 -> Arbitrary.arbitrary[A].map(GraphExpr.Vertex(_)),
          3 -> Gen.zip(child, child).map(GraphExpr.Overlay(_, _)),
          3 -> Gen.zip(child, child).map(GraphExpr.Join(_, _))
        )
    }

  def digraphExpr[A](using Arbitrary[A]): Gen[DigraphExpr[A]] =
    Gen.sized { size =>
      if size <= 0 then
        Gen.oneOf(
          Gen.const(DigraphExpr.Empty),
          Arbitrary.arbitrary[A].map(DigraphExpr.Vertex(_))
        )
      else
        val child = Gen.resize(size / 2, digraphExpr[A])
        Gen.frequency(
          1 -> Gen.const(DigraphExpr.Empty),
          3 -> Arbitrary.arbitrary[A].map(DigraphExpr.Vertex(_)),
          3 -> Gen.zip(child, child).map(DigraphExpr.Overlay(_, _)),
          3 -> Gen.zip(child, child).map(DigraphExpr.Connect(_, _))
        )
    }

  given [A: Arbitrary]: Arbitrary[GraphExpr[A]] =
    Arbitrary(graphExpr[A])

  given [A: Arbitrary]: Arbitrary[DigraphExpr[A]] =
    Arbitrary(digraphExpr[A])

  given [A: Eq]: Eq[GraphExpr[A]] with
    def eqv(left: GraphExpr[A], right: GraphExpr[A]): Boolean =
      (left, right) match
        case (GraphExpr.Empty, GraphExpr.Empty) =>
          true
        case (GraphExpr.Vertex(left), GraphExpr.Vertex(right)) =>
          Eq[A].eqv(left, right)
        case (GraphExpr.Overlay(ll, lr), GraphExpr.Overlay(rl, rr)) =>
          eqv(ll, rl) && eqv(lr, rr)
        case (GraphExpr.Join(ll, lr), GraphExpr.Join(rl, rr)) =>
          eqv(ll, rl) && eqv(lr, rr)
        case _ =>
          false

  given [A: Eq]: Eq[DigraphExpr[A]] with
    def eqv(left: DigraphExpr[A], right: DigraphExpr[A]): Boolean =
      (left, right) match
        case (DigraphExpr.Empty, DigraphExpr.Empty) =>
          true
        case (DigraphExpr.Vertex(left), DigraphExpr.Vertex(right)) =>
          Eq[A].eqv(left, right)
        case (DigraphExpr.Overlay(ll, lr), DigraphExpr.Overlay(rl, rr)) =>
          eqv(ll, rl) && eqv(lr, rr)
        case (DigraphExpr.Connect(lf, lt), DigraphExpr.Connect(rf, rt)) =>
          eqv(lf, rf) && eqv(lt, rt)
        case _ =>
          false
