package graph4s.gale

import cats.Order
import gale.sparse.CSR
import gale.spectral.{Eigen, EigenSelection}
import graph4s.algorithms.GraphAlgorithms
import graph4s.indexed.IndexedGraph
import munit.FunSuite

final class GraphOperatorsSuite extends FunSuite:
  private val tolerance = 1e-10

  test("affinity wrappers validate their distinct numerical domains"):
    assert(SignedAffinity.from(-0.5).isRight)
    assert(SignedAffinity.from(Double.NaN).isLeft)
    assert(NonNegativeAffinity.from(0.0).isRight)
    assert(NonNegativeAffinity.from(-0.01).isLeft)
    assert(PositiveAffinity.from(0.0).isLeft)
    assert(PositiveAffinity.from(0.01).isRight)

  test("topology adjacency preserves explicit zero edges while weighted CSR drops zeros"):
    val graph = TestGraphFixtures.undirected(
      Vector(
        ("a", "b", NonNegativeAffinity.unsafe(2.0)),
        ("b", "c", NonNegativeAffinity.unsafe(0.0))
      )
    )

    val topology = dense(graph.index.topologyAdjacency.matrix)
    val weighted = dense(graph.weights.weightedAdjacency.toOption.get.matrix)

    assertMatrixEquals(
      topology,
      Vector(
        Vector(0.0, 1.0, 0.0, 0.0),
        Vector(1.0, 0.0, 1.0, 0.0),
        Vector(0.0, 1.0, 0.0, 0.0),
        Vector(0.0, 0.0, 0.0, 0.0)
      )
    )
    assertMatrixEquals(
      weighted,
      Vector(
        Vector(0.0, 2.0, 0.0, 0.0),
        Vector(2.0, 0.0, 0.0, 0.0),
        Vector(0.0, 0.0, 0.0, 0.0),
        Vector(0.0, 0.0, 0.0, 0.0)
      )
    )
    assertEquals(
      graph.index.degreeSignal.values.toSeq.toVector,
      Vector(1.0, 2.0, 1.0, 0.0)
    )
    assertEquals(
      graph.weights.strength.toOption.get.values.toSeq.toVector,
      Vector(2.0, 2.0, 0.0, 0.0)
    )

  test("signed weighted adjacency is symmetric"):
    val graph = TestGraphFixtures.undirected(
      Vector(
        ("a", "b", SignedAffinity.unsafe(-0.4)),
        ("a", "d", SignedAffinity.unsafe(0.7))
      )
    )
    val adjacency = dense(graph.weights.weightedAdjacency.toOption.get.matrix)

    assertEqualsDouble(adjacency(0)(1), -0.4, tolerance)
    assertEqualsDouble(adjacency(1)(0), -0.4, tolerance)
    assertEqualsDouble(adjacency(0)(3), 0.7, tolerance)
    assertEqualsDouble(adjacency(3)(0), 0.7, tolerance)

  test("directed degree and strength retain direction"):
    val graph = TestGraphFixtures.directed(
      Vector(
        ("a", "b", SignedAffinity.unsafe(2.0)),
        ("a", "c", SignedAffinity.unsafe(-1.0)),
        ("d", "a", SignedAffinity.unsafe(4.0))
      )
    )

    assertEquals(
      graph.index.outDegreeSignal.values.toSeq.toVector,
      Vector(2.0, 0.0, 0.0, 1.0)
    )
    assertEquals(
      graph.index.inDegreeSignal.values.toSeq.toVector,
      Vector(1.0, 1.0, 1.0, 0.0)
    )
    assertEquals(
      graph.weights.outStrength.toOption.get.values.toSeq.toVector,
      Vector(1.0, 0.0, 0.0, 4.0)
    )
    assertEquals(
      graph.weights.inStrength.toOption.get.values.toSeq.toVector,
      Vector(4.0, 2.0, -1.0, 0.0)
    )

  test("incidence columns use the indexed graph's canonical edge order"):
    val graph = TestGraphFixtures.undirected(
      Vector(("d", "b", 3), ("c", "a", 2), ("b", "a", 1))
    )
    val incidence = graph.index.incidence
    val endpoints =
      graph.index.edges.map: edge =>
        val (left, right) = graph.index.endpoints(edge)
        graph.index.label(left) -> graph.index.label(right)

    assertEquals(endpoints.toVector, Vector("a" -> "b", "a" -> "c", "b" -> "d"))
    assertMatrixEquals(
      dense(incidence.matrix),
      Vector(
        Vector(-1.0, -1.0, 0.0),
        Vector(1.0, 0.0, -1.0),
        Vector(0.0, 1.0, 0.0),
        Vector(0.0, 0.0, 1.0)
      )
    )

  test("combinatorial Laplacian equals D minus A and is positive semidefinite"):
    val graph = TestGraphFixtures.undirected(
      Vector(
        ("a", "b", NonNegativeAffinity.unsafe(2.0)),
        ("a", "c", NonNegativeAffinity.unsafe(1.0)),
        ("b", "d", NonNegativeAffinity.unsafe(3.0))
      )
    )
    val laplacian = dense(graph.weights.combinatorialLaplacian.toOption.get.matrix)
    val expected = Vector(
      Vector(3.0, -2.0, -1.0, 0.0),
      Vector(-2.0, 5.0, 0.0, -3.0),
      Vector(-1.0, 0.0, 1.0, 0.0),
      Vector(0.0, -3.0, 0.0, 3.0)
    )

    assertMatrixEquals(laplacian, expected)
    laplacian.foreach(row => assertEqualsDouble(row.sum, 0.0, tolerance))
    Vector(
      Vector(1.0, -2.0, 0.5, 3.0),
      Vector(-4.0, 1.0, 2.0, -0.25),
      Vector(0.0, 0.0, 0.0, 0.0)
    ).foreach: vector =>
      assert(quadraticForm(laplacian, vector) >= -tolerance)

  test("oriented incidence satisfies L equals B W B transpose"):
    val numericWeights = Vector(2.0, 1.0, 3.0)
    val graph = TestGraphFixtures.undirected(
      Vector(
        ("a", "b", NonNegativeAffinity.unsafe(numericWeights(0))),
        ("a", "c", NonNegativeAffinity.unsafe(numericWeights(1))),
        ("b", "d", NonNegativeAffinity.unsafe(numericWeights(2)))
      )
    )
    val b = dense(graph.index.incidence.matrix)
    val reconstructed = multiplyWeightedIncidence(b, numericWeights)
    val laplacian = dense(graph.weights.combinatorialLaplacian.toOption.get.matrix)

    assertMatrixEquals(reconstructed, laplacian)

  test("weighted Laplacian nullity follows positive-weight support"):
    given Order[String] = Order.fromComparable
    val graph = TestGraphFixtures.undirected(
      Vector(
        ("a", "b", NonNegativeAffinity.unsafe(1.0)),
        ("b", "c", NonNegativeAffinity.unsafe(0.0)),
        ("c", "d", NonNegativeAffinity.unsafe(2.0))
      )
    )
    val support = TestGraphFixtures.undirected(
      Vector(("a", "b", ()), ("c", "d", ()))
    )
    val eigen =
      Eigen
        .eigSymmetric(
          graph.weights.combinatorialLaplacian.toOption.get.matrix.toDense(),
          EigenSelection.All
        )
        .toOption
        .get
    val nullity = eigen.eigenvalues.toSeq.count(value => Math.abs(value) < 1e-9)

    assertEquals(GraphAlgorithms.connectedComponents(graph.topology).components.size, 1)
    assertEquals(GraphAlgorithms.connectedComponents(support.topology).components.size, 2)
    assertEquals(nullity, 2)

  test("normalized Laplacians make zero-strength behavior explicit"):
    val graph = TestGraphFixtures.undirected(
      Vector(("a", "b", NonNegativeAffinity.unsafe(2.0)))
    )
    val symmetricZero = dense(
      graph.weights
        .normalizedLaplacian(
          NormalizedLaplacian.Symmetric,
          ZeroStrengthPolicy.KeepZeroRow
        )
        .toOption
        .get
        .matrix
    )
    val symmetricIdentity = dense(
      graph.weights
        .normalizedLaplacian(
          NormalizedLaplacian.Symmetric,
          ZeroStrengthPolicy.IdentityOnZeroStrength
        )
        .toOption
        .get
        .matrix
    )
    val randomWalk = dense(
      graph.weights
        .normalizedLaplacian(
          NormalizedLaplacian.RandomWalk,
          ZeroStrengthPolicy.KeepZeroRow
        )
        .toOption
        .get
        .matrix
    )

    assertMatrixEquals(
      symmetricZero,
      Vector(
        Vector(1.0, -1.0, 0.0, 0.0),
        Vector(-1.0, 1.0, 0.0, 0.0),
        Vector(0.0, 0.0, 0.0, 0.0),
        Vector(0.0, 0.0, 0.0, 0.0)
      )
    )
    assertEqualsDouble(symmetricIdentity(2)(2), 1.0, tolerance)
    assertEqualsDouble(symmetricIdentity(3)(3), 1.0, tolerance)
    assertMatrixEquals(randomWalk, symmetricZero)
    graph.weights.normalizedLaplacian(
      NormalizedLaplacian.Symmetric,
      ZeroStrengthPolicy.Error
    ) match
      case Left(GraphLinalgError.ZeroStrengthVertex("c")) => ()
      case other => fail(s"expected zero-strength error for c, got $other")

  test("normalized random-walk Laplacian uses source strengths"):
    val graph = TestGraphFixtures.undirected(
      Vector(
        ("a", "b", NonNegativeAffinity.unsafe(1.0)),
        ("b", "c", NonNegativeAffinity.unsafe(3.0))
      )
    )
    val laplacian = dense(
      graph.weights
        .normalizedLaplacian(
          NormalizedLaplacian.RandomWalk,
          ZeroStrengthPolicy.KeepZeroRow
        )
        .toOption
        .get
        .matrix
    )

    assertEqualsDouble(laplacian(0)(1), -1.0, tolerance)
    assertEqualsDouble(laplacian(1)(0), -0.25, tolerance)
    assertEqualsDouble(laplacian(1)(2), -0.75, tolerance)
    assertEqualsDouble(laplacian(2)(1), -1.0, tolerance)

  test("invalid capability implementations are checked at the numerical boundary"):
    given NonNegativeAdjacencyWeight[Double] with
      def weight(edge: Double): Double = edge

    val graph = TestGraphFixtures.undirected(Vector(("a", "b", -1.0)))
    graph.weights.combinatorialLaplacian match
      case Left(
            GraphLinalgError.InvalidWeight(
              0,
              "a",
              "b",
              -1.0,
              WeightRequirement.NonNegative
            )
          ) =>
        ()
      case other =>
        fail(s"expected invalid non-negative weight, got $other")

  test("reindexing gives permutation-similar adjacency and Laplacian operators"):
    val edges = Vector(
      ("a", "b", NonNegativeAffinity.unsafe(2.0)),
      ("a", "d", NonNegativeAffinity.unsafe(4.0)),
      ("c", "d", NonNegativeAffinity.unsafe(1.0))
    )
    val graph = TestGraphFixtures.undirected(edges)
    val reordered = TestGraphFixtures.undirected(edges, Vector("d", "b", "a", "c"))
    val originalAdjacency = dense(graph.weights.weightedAdjacency.toOption.get.matrix)
    val reorderedAdjacency = dense(reordered.weights.weightedAdjacency.toOption.get.matrix)
    val originalLaplacian = dense(graph.weights.combinatorialLaplacian.toOption.get.matrix)
    val reorderedLaplacian = dense(reordered.weights.combinatorialLaplacian.toOption.get.matrix)

    assertKeyPermutation(
      originalAdjacency,
      labels(graph.index),
      reorderedAdjacency,
      labels(reordered.index)
    )
    assertKeyPermutation(
      originalLaplacian,
      labels(graph.index),
      reorderedLaplacian,
      labels(reordered.index)
    )

  private def labels(index: IndexedGraph[String]): Vector[String] =
    index.vertices.map(index.label).toVector

  private def dense(matrix: CSR): Vector[Vector[Double]] =
    Vector.tabulate(matrix.rows): row =>
      Vector.tabulate(matrix.cols)(col => matrix(row, col))

  private def assertMatrixEquals(
      actual: Vector[Vector[Double]],
      expected: Vector[Vector[Double]]
  ): Unit =
    assertEquals(actual.length, expected.length)
    actual.indices.foreach: row =>
      assertEquals(actual(row).length, expected(row).length)
      actual(row).indices.foreach: col =>
        assertEqualsDouble(actual(row)(col), expected(row)(col), tolerance)

  private def quadraticForm(
      matrix: Vector[Vector[Double]],
      vector: Vector[Double]
  ): Double =
    vector.indices
      .map: row =>
        vector.indices.map(col => vector(row) * matrix(row)(col) * vector(col)).sum
      .sum

  private def multiplyWeightedIncidence(
      incidence: Vector[Vector[Double]],
      weights: Vector[Double]
  ): Vector[Vector[Double]] =
    Vector.tabulate(incidence.length, incidence.length): (row, col) =>
      weights.indices
        .map(edge => incidence(row)(edge) * weights(edge) * incidence(col)(edge))
        .sum

  private def assertKeyPermutation[K](
      original: Vector[Vector[Double]],
      originalKeys: Vector[K],
      reordered: Vector[Vector[Double]],
      reorderedKeys: Vector[K]
  ): Unit =
    reorderedKeys.indices.foreach: row =>
      reorderedKeys.indices.foreach: col =>
        val sourceRow = originalKeys.indexOf(reorderedKeys(row))
        val sourceCol = originalKeys.indexOf(reorderedKeys(col))
        assertEqualsDouble(
          reordered(row)(col),
          original(sourceRow)(sourceCol),
          tolerance
        )
