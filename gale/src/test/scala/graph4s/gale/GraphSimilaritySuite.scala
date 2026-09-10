package graph4s.gale

import cats.{Hash, Order}
import gale.linalg.{DMat, Matrix}
import gale.spectral.{Eigen, EigenSelection}
import graph4s.{Graph, Link}
import graph4s.data.{EdgeField, IndexedEdgeField}
import graph4s.indexed.{IndexedGraph, VertexOrder}
import munit.FunSuite

final class GraphSimilaritySuite extends FunSuite:
  private val tolerance = 1e-8

  test("heat and diffusion diagonal features match the analytic two-vertex graph"):
    val graph =
      UndirectedFixture(
        Vector("a", "b"),
        Vector(("a", "b", NonNegativeAffinity.unsafe(1.0))),
        Vector("a", "b")
      )
    val spectrum = graph.weights.vertexSpectrum(2).toOption.get
    val heat =
      SpectralDiagonalFeature
        .from(Vector(0.0, 1.0))
        .toOption
        .get(spectrum)
        .toOption
        .get
    val diffusion =
      SpectralDiagonalFeature
        .from(Vector(1.0), SpectralDiagonalKind.DiffusionEnergy)
        .toOption
        .get(spectrum)
        .toOption
        .get

    assertEqualsDouble(heat.values(0, 0), 1.0, tolerance)
    assertEqualsDouble(heat.values(1, 0), 1.0, tolerance)
    assertEqualsDouble(
      heat.values(0, 1),
      0.5 * (1.0 + Math.exp(-2.0)),
      tolerance
    )
    assertEqualsDouble(heat.values(1, 1), heat.values(0, 1), tolerance)
    assertEqualsDouble(
      diffusion.values(0, 0),
      0.5 * (1.0 + Math.exp(-4.0)),
      tolerance
    )

  test("feature similarities require ordered indices and support explicit alignment"):
    val edges = Vector(("a", "b", 1.0), ("b", "c", 1.0), ("c", "d", 2.0))
    val graph = weightedGraph(edges)
    val reordered = weightedGraph(edges, Vector("d", "b", "a", "c"))
    val featurePlan =
      SpectralDiagonalFeature.from(Vector(0.25, 1.0)).toOption.get
    val left =
      featurePlan(graph.weights.vertexSpectrum(4).toOption.get).toOption.get
    val right =
      featurePlan(reordered.weights.vertexSpectrum(4).toOption.get).toOption.get
    val similarity = LinearFeatureSimilarity[String]()

    assert(similarity(left, right).isLeft)
    val aligned = right.alignTo(left.index).toOption.get
    assertMatrixClose(aligned.values, left.values, tolerance)
    assertEqualsDouble(
      similarity(left, aligned).toOption.get,
      similarity(left, left).toOption.get,
      tolerance
    )

  test("linear and RBF feature similarities form PSD Gram matrices"):
    val featurePlan =
      SpectralDiagonalFeature.from(Vector(0.1, 0.5, 1.5)).toOption.get
    val graphs = Vector(
      weightedGraph(Vector(("a", "b", 1.0), ("b", "c", 1.0), ("c", "d", 1.0))),
      weightedGraph(Vector(("a", "b", 2.0), ("b", "c", 0.5), ("c", "d", 1.5))),
      weightedGraph(
        Vector(
          ("a", "b", 1.0),
          ("b", "c", 1.0),
          ("c", "d", 1.0),
          ("d", "a", 1.0)
        )
      )
    )
    val features = graphs.map: graph =>
      featurePlan(graph.weights.vertexSpectrum(4).toOption.get).toOption.get
    val linear = LinearFeatureSimilarity[String]()
    val rbf = RbfFeatureSimilarity.from[String](0.75).toOption.get

    assert(linear.psdStatus.isInstanceOf[PsdStatus.KnownPsd])
    assert(rbf.psdStatus.isInstanceOf[PsdStatus.KnownPsd])
    assertPsd(gram(features, linear))
    assertPsd(gram(features, rbf))

  test("feature vectorization is row-major and agrees with linear similarity"):
    val featurePlan =
      SpectralDiagonalFeature.from(Vector(0.2, 1.0)).toOption.get
    val graph =
      weightedGraph(Vector(("a", "b", 2.0), ("b", "c", 1.0), ("c", "d", 0.5)))
    val feature =
      featurePlan(graph.weights.vertexSpectrum(4).toOption.get).toOption.get
    val flattened = feature.toVector.toSeq.toVector
    val expected =
      Vector.tabulate(feature.values.rows * feature.values.cols): linear =>
        feature.values(linear / feature.values.cols, linear % feature.values.cols)
    val dot = flattened.indices.map(index => flattened(index) * flattened(index)).sum
    val similarity =
      LinearFeatureSimilarity[String]()(feature, feature).toOption.get

    assertEquals(flattened, expected)
    assertEqualsDouble(similarity, dot, tolerance)

  test("unjustified similarities remain explicitly outside the PSD path"):
    val status = NegativeEuclideanFeatureSimilarity[String]().psdStatus

    status match
      case PsdStatus.NotEstablished(reason) =>
        assert(reason.contains("not a declared PSD kernel"))
      case other =>
        fail(s"expected unestablished PSD status, got $other")

  test("feature and similarity parameters fail at construction boundaries"):
    assert(SpectralDiagonalFeature.from(Vector.empty).isLeft)
    assert(SpectralDiagonalFeature.from(Vector(-1.0)).isLeft)
    assert(SpectralDiagonalFeature.from(Vector(Double.NaN)).isLeft)
    assert(RbfFeatureSimilarity.from[String](0.0).isLeft)

  test("feature basis comparison honors the indexed graph Hash equality"):
    final case class Label(value: String, instance: Int)
    given Hash[Label] = Hash.by(_.value)
    given Order[Label] = Order.by(_.value)

    def features(instance: Int): VertexFeatureSet[Label, ? <: IndexedGraph[Label]] =
      val left = Label("a", instance)
      val right = Label("b", instance)
      val topology =
        Graph
          .of(Vector(left, right), Vector(Link(left, right)))
          .fold(errors => fail(errors.toString), identity)
      val index =
        IndexedGraph
          .from(topology, VertexOrder.by(Order[Label]))
          .fold(errors => fail(errors.toString), identity)
      val field =
        EdgeField.total(topology)(_ => NonNegativeAffinity.unsafe(1.0))
      val weights =
        IndexedEdgeField.from(index, field).fold(error => fail(error.toString), identity)
      val spectrum =
        weights.vertexSpectrum(2).fold(error => fail(error.message), identity)
      SpectralDiagonalFeature
        .from(Vector(0.5))
        .flatMap(_(spectrum))
        .fold(error => fail(error.message), identity)

    val similarity = LinearFeatureSimilarity[Label]()
    assert(similarity(features(1), features(2)).isRight)

  private def weightedGraph(
      edges: Vector[(String, String, Double)],
      order: Vector[String] = TestGraphFixtures.vertices
  ): UndirectedFixture[NonNegativeAffinity] =
    TestGraphFixtures.undirected(
      edges.map: (from, to, value) =>
        (from, to, NonNegativeAffinity.unsafe(value)),
      order
    )

  private def gram(
      features: Vector[VertexFeatureSet[String, ? <: IndexedGraph[String]]],
      similarity: GraphSimilarity[VertexFeatureSet[String, ? <: IndexedGraph[String]]]
  ): DMat =
    Matrix.dense(
      features.length,
      features.length,
      features.flatMap(left => features.map(right => similarity(left, right).toOption.get))
    )

  private def assertPsd(matrix: DMat): Unit =
    val eigen = Eigen.eigSymmetric(matrix, EigenSelection.All).toOption.get
    assert(eigen.eigenvalues.toSeq.forall(_ >= -tolerance), clue = eigen.eigenvalues.toSeq)

  private def assertMatrixClose(
      actual: DMat,
      expected: DMat,
      tolerance: Double
  ): Unit =
    assertEquals(actual.rows, expected.rows)
    assertEquals(actual.cols, expected.cols)
    var row = 0
    while row < actual.rows do
      var col = 0
      while col < actual.cols do
        assertEqualsDouble(actual(row, col), expected(row, col), tolerance)
        col += 1
      row += 1
