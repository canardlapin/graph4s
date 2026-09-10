package graph4s.gale

import graph4s.indexed.IndexedGraph
import munit.FunSuite
import scala.compiletime.testing.typeCheckErrors

final class GraphSpectralSuite extends FunSuite:
  private val tolerance = 1e-8

  test("path graph spectrum matches analytic combinatorial eigenvalues and retains its index"):
    val graph =
      weightedGraph(Vector(("a", "b", 1.0), ("b", "c", 1.0), ("c", "d", 1.0)))
    val spectrum = graph.weights.vertexSpectrum(4).toOption.get
    val expected = Vector(
      0.0,
      2.0 - Math.sqrt(2.0),
      2.0,
      2.0 + Math.sqrt(2.0)
    )

    assert(spectrum.index eq graph.index)
    assertVectorClose(spectrum.result.eigenvalues.toSeq.toVector, expected, tolerance)
    assert(spectrum.result.diagnostics.residuals.toSeq.forall(_ < tolerance))
    assertEquals(spectrum.expectedZeroEigenvalueMultiplicity, 1)
    assertEquals(
      spectrum.support.components.map(_.vertices.toVector),
      Vector(Vector("a", "b", "c", "d"))
    )

  test("zero-weight support determines combinatorial nullity"):
    val graph = weightedGraph(
      Vector(
        ("a", "b", 1.0),
        ("b", "c", 0.0),
        ("c", "d", 2.0)
      )
    )
    val spectrum = graph.weights.vertexSpectrum(4).toOption.get
    val nullity =
      spectrum.result.eigenvalues.toSeq.count(value => Math.abs(value) < tolerance)

    assertEquals(spectrum.support.zeroWeightEdgeCount, 1)
    assertEquals(
      spectrum.support.components.map(_.vertices.toVector),
      Vector(Vector("a", "b"), Vector("c", "d"))
    )
    assertEquals(spectrum.expectedZeroEigenvalueMultiplicity, 2)
    assertEquals(nullity, 2)

  test("normalized isolate policy changes expected nullity explicitly"):
    val graph = weightedGraph(Vector(("a", "b", 2.0)))
    val keep =
      graph.weights
        .vertexSpectrum(
          4,
          SpectralLaplacian.SymmetricNormalized(ZeroStrengthPolicy.KeepZeroRow)
        )
        .toOption
        .get
    val identity =
      graph.weights
        .vertexSpectrum(
          4,
          SpectralLaplacian.SymmetricNormalized(
            ZeroStrengthPolicy.IdentityOnZeroStrength
          )
        )
        .toOption
        .get

    assertEquals(keep.support.zeroStrengthVertices, Vector("c", "d"))
    assertEquals(keep.expectedZeroEigenvalueMultiplicity, 3)
    assertEquals(identity.expectedZeroEigenvalueMultiplicity, 1)
    assertEquals(
      keep.result.eigenvalues.toSeq.count(value => Math.abs(value) < tolerance),
      3
    )
    assertEquals(
      identity.result.eigenvalues.toSeq.count(value => Math.abs(value) < tolerance),
      1
    )
    graph.weights.vertexSpectrum(
      2,
      SpectralLaplacian.SymmetricNormalized(ZeroStrengthPolicy.Error)
    ) match
      case Left(GraphLinalgError.ZeroStrengthVertex("c")) => ()
      case other => fail(s"expected zero-strength vertex c, got $other")

  test("spectral embedding drops expected nullspace and retains diagnostics"):
    val graph =
      weightedGraph(Vector(("a", "b", 1.0), ("b", "c", 1.0), ("c", "d", 1.0)))
    val embedding = graph.weights.spectralEmbedding(2).toOption.get

    assert(embedding.index eq graph.index)
    assertEquals(embedding.coordinates.rows, 4)
    assertEquals(embedding.coordinates.cols, 2)
    assertEquals(embedding.droppedEigenvectors, 1)
    assertVectorClose(
      embedding.eigenvalues.toSeq.toVector,
      Vector(2.0 - Math.sqrt(2.0), 2.0),
      tolerance
    )
    assert(embedding.residualNorms.toSeq.forall(_ < tolerance))

  test("cycle embedding is invariant under reindexing by repeated-eigenspace geometry"):
    val edges = Vector(
      ("a", "b", 1.0),
      ("b", "c", 1.0),
      ("c", "d", 1.0),
      ("d", "a", 1.0)
    )
    val graph = weightedGraph(edges)
    val reordered = weightedGraph(edges, Vector("d", "b", "a", "c"))
    val first = graph.weights.spectralEmbedding(2).toOption.get
    val second = reordered.weights.spectralEmbedding(2).toOption.get

    assertVectorClose(first.eigenvalues.toSeq.toVector, Vector(2.0, 2.0), tolerance)
    assertVectorClose(
      second.eigenvalues.toSeq.toVector,
      first.eigenvalues.toSeq.toVector,
      tolerance
    )
    TestGraphFixtures.vertices.foreach: left =>
      TestGraphFixtures.vertices.foreach: right =>
        assertEqualsDouble(
          squaredDistance(first, left, right),
          squaredDistance(second, left, right),
          tolerance
        )

  test("largest spectrum remains available without changing support metadata"):
    val graph =
      weightedGraph(Vector(("a", "b", 1.0), ("b", "c", 1.0), ("c", "d", 1.0)))
    val spectrum =
      graph.weights
        .vertexSpectrum(1, spectrum = SpectralEnd.Largest)
        .toOption
        .get

    assertEqualsDouble(
      spectrum.result.eigenvalues(0),
      2.0 + Math.sqrt(2.0),
      tolerance
    )
    assertEquals(spectrum.expectedZeroEigenvalueMultiplicity, 1)

  test("largest selection retains Gale's ascending-algebraic result layout"):
    val graph =
      weightedGraph(Vector(("a", "b", 1.0), ("b", "c", 1.0), ("c", "d", 1.0)))
    val spectrum =
      graph.weights
        .vertexSpectrum(2, spectrum = SpectralEnd.Largest)
        .toOption
        .get

    assertVectorClose(
      spectrum.result.eigenvalues.toSeq.toVector,
      Vector(2.0, 2.0 + Math.sqrt(2.0)),
      tolerance
    )

  test("spectrum rank is validated at the indexed boundary"):
    val graph = weightedGraph(Vector(("a", "b", 1.0)))

    assertEquals(
      graph.weights.vertexSpectrum(0),
      Left(GraphLinalgError.InvalidSpectrumRank(0, 4))
    )
    assertEquals(
      graph.weights.vertexSpectrum(5),
      Left(GraphLinalgError.InvalidSpectrumRank(5, 4))
    )

  test("embedding dimensions fail when geometry cannot fit after nullspace removal"):
    val edgeless = weightedGraph(Vector.empty)

    assert(edgeless.weights.spectralEmbedding(0).isLeft)
    edgeless.weights.spectralEmbedding(1) match
      case Left(GraphLinalgError.InvalidEmbeddingDimensions(1, 4, 4)) => ()
      case other => fail(s"expected embedding dimension error, got $other")

  test("numerical artifacts retain their exact indexed snapshot at compile time"):
    val errors = typeCheckErrors(
      """
        import cats.{Hash, Order}
        import graph4s.*
        import graph4s.syntax.*
        import graph4s.data.*
        import graph4s.gale.*
        import graph4s.indexed.*
        import graph4s.indexed.syntax.*
        given Hash[String] = Hash.fromUniversalHashCode
        given Order[String] = Order.fromComparable
        val topology = Graph.fromEdges("a" -- "b").toOption.get
        val first = topology.indexed(VertexOrder.by(Order[String])).toOption.get
        val second = topology.indexed(VertexOrder.by(Order[String])).toOption.get
        val field = EdgeField.total(topology)(_ => NonNegativeAffinity.unsafe(1.0))
        val weights = IndexedEdgeField.from(first, field).toOption.get
        val spectrum = weights.vertexSpectrum(2).toOption.get
        val incidence = first.incidence
        val featurePlan = SpectralDiagonalFeature.from(Vector(0.5)).toOption.get
        val features = featurePlan(spectrum).toOption.get
        val wrongSpectrum: VertexSpectrum[String, second.type] = spectrum
        val wrongIncidence: GraphIncidenceOperator[String, second.type] = incidence
        val wrongFeatures: VertexFeatureSet[String, second.type] = features
      """
    )

    assert(errors.length >= 3)

  private def weightedGraph(
      edges: Vector[(String, String, Double)],
      order: Vector[String] = TestGraphFixtures.vertices
  ): UndirectedFixture[NonNegativeAffinity] =
    TestGraphFixtures.undirected(
      edges.map: (from, to, value) =>
        (from, to, NonNegativeAffinity.unsafe(value)),
      order
    )

  private def squaredDistance[I <: IndexedGraph[String]](
      embedding: SpectralEmbedding[String, I],
      left: String,
      right: String
  ): Double =
    val leftRow = embedding.index.ordinal(embedding.index.vertex(left).get)
    val rightRow = embedding.index.ordinal(embedding.index.vertex(right).get)
    var total = 0.0
    var col = 0
    while col < embedding.coordinates.cols do
      val difference =
        embedding.coordinates(leftRow, col) - embedding.coordinates(rightRow, col)
      total += difference * difference
      col += 1
    total

  private def assertVectorClose(
      actual: Vector[Double],
      expected: Vector[Double],
      tolerance: Double
  ): Unit =
    assertEquals(actual.length, expected.length)
    actual
      .zip(expected)
      .foreach: (left, right) =>
        assertEqualsDouble(left, right, tolerance)
