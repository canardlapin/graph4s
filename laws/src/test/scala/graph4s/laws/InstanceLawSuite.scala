package graph4s.laws

import cats.Hash
import cats.kernel.laws.discipline.{BoundedSemilatticeTests, HashTests, PartialOrderTests}
import cats.laws.discipline.FunctorTests
import graph4s.{Arc, Digraph, Edge, Graph, VertexSet}
import graph4s.expr.{DigraphExpr, GraphExpr}
import munit.DisciplineSuite

final class InstanceLawSuite extends DisciplineSuite:
  import ExpressionGenerators.given
  import GraphGenerators.given

  given Hash[Int] = Hash.fromUniversalHashCode

  checkAll(
    "GraphExpr functor laws",
    FunctorTests[GraphExpr].functor[Int, Int, String]
  )

  checkAll(
    "DigraphExpr functor laws",
    FunctorTests[DigraphExpr].functor[Int, Int, String]
  )

  checkAll(
    "Edge hash laws",
    HashTests[Edge[Int]].hash
  )

  checkAll(
    "Arc hash laws",
    HashTests[Arc[Int]].hash
  )

  checkAll(
    "VertexSet hash laws",
    HashTests[VertexSet[Int]].hash
  )

  checkAll(
    "Graph partial order laws",
    PartialOrderTests[Graph[Int]].partialOrder
  )

  checkAll(
    "Digraph partial order laws",
    PartialOrderTests[Digraph[Int]].partialOrder
  )

  checkAll(
    "Digraph bounded semilattice laws",
    BoundedSemilatticeTests[Digraph[Int]].boundedSemilattice
  )
