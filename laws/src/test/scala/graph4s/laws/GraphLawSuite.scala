package graph4s.laws

import cats.{Hash, Order}
import cats.kernel.laws.discipline.BoundedSemilatticeTests
import graph4s.Graph
import munit.DisciplineSuite

final class GraphLawSuite extends DisciplineSuite:
  import GraphGenerators.given

  given Hash[Int] = Hash.fromUniversalHashCode
  given Order[Int] = Order.fromOrdering

  checkAll(
    "Graph structural laws",
    GraphLaws.structural(GraphGenerators.smallIntGraph)
  )

  checkAll(
    "Digraph structural laws",
    GraphLaws.digraphStructural(GraphGenerators.smallIntDigraph)
  )

  checkAll(
    "Algorithm reference conformance",
    AlgorithmLaws.referenceConformance(
      GraphGenerators.smallIntGraph,
      GraphGenerators.smallIntDigraph
    )
  )

  checkAll(
    "Graph bounded semilattice laws",
    BoundedSemilatticeTests[Graph[Int]].boundedSemilattice
  )
