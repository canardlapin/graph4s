package graph4s.gale

import gale.linalg.{DMat, DVec, Matrix, Vec}
import graph4s.indexed.IndexedGraph

final case class GraphFeatureSpec private[gale] (name: String, parameters: Vector[Double]):
  require(name.trim.nonEmpty, "graph feature name must be non-empty")
  require(parameters.forall(_.isFinite), "graph feature parameters must be finite")

sealed trait GraphSimilarityError:
  def message: String

object GraphSimilarityError:
  final case class InvalidParameter(name: String, value: Double, reason: String)
      extends GraphSimilarityError:
    def message: String =
      s"invalid $name $value: $reason"

  final case class BasisMismatch[V](left: Vector[V], right: Vector[V]) extends GraphSimilarityError:
    def message: String =
      s"graph feature basis mismatch: left=[${left.mkString(",")}] right=[${right.mkString(",")}]"

  final case class FeatureMismatch(left: GraphFeatureSpec, right: GraphFeatureSpec)
      extends GraphSimilarityError:
    def message: String =
      s"graph feature specification mismatch: left=${left.name}${left.parameters} right=${right.name}${right.parameters}"

  final case class NonFiniteFeature(index: Int, value: Double) extends GraphSimilarityError:
    def message: String =
      s"graph feature value at linear index $index is not finite: $value"

enum PsdStatus:
  case KnownPsd(construction: String)
  case NotEstablished(reason: String)

trait GraphSimilarity[-A]:
  def name: String
  def psdStatus: PsdStatus
  def apply(left: A, right: A): Either[GraphSimilarityError, Double]

final class VertexFeatureSet[V, I <: IndexedGraph[V]] private[gale] (
    val index: I,
    val values: DMat,
    val spec: GraphFeatureSpec
):
  require(values.rows == index.vertexCount, "vertex feature rows must match the index")

  def featureCount: Int =
    values.cols

  def toVector: DVec =
    Vec.tabulate(values.rows * values.cols): linear =>
      values(linear / values.cols, linear % values.cols)

  /** Align rows by graph labels.
    *
    * This establishes numerical label alignment only; domain-specific layers remain responsible for
    * scientific provenance compatibility.
    */
  def alignTo[J <: IndexedGraph[V]](
      target: J
  ): Either[GraphSimilarityError, VertexFeatureSet[V, target.type]] =
    val sourceLabels = labels(index)
    val targetLabels = labels(target)
    val sourceRows =
      targetLabels.map: targetLabel =>
        index
          .vertex(targetLabel)
          .filter(sourceVertex => target.vertexHash.eqv(index.label(sourceVertex), targetLabel))
          .map(index.ordinal)
    if index.vertexCount != target.vertexCount ||
      sourceRows.exists(_.isEmpty)
    then
      Left(
        GraphSimilarityError.BasisMismatch(
          sourceLabels,
          targetLabels
        )
      )
    else
      val aligned = Matrix.newBuilder(target.vertexCount, values.cols)
      var row = 0
      val sourceOrdinals = sourceRows.iterator.flatten
      while row < target.vertexCount do
        val sourceRow = sourceOrdinals.next()
        var col = 0
        while col < values.cols do
          aligned(row, col) = values(sourceRow, col)
          col += 1
        row += 1
      Right(new VertexFeatureSet[V, target.type](target, aligned.result(), spec))

enum SpectralDiagonalKind:
  case HeatKernel
  case DiffusionEnergy

  private[gale] def exponentFactor: Double =
    this match
      case HeatKernel      => 1.0
      case DiffusionEnergy => 2.0

  private[gale] def label: String =
    this match
      case HeatKernel      => "heat-kernel-diagonal"
      case DiffusionEnergy => "diffusion-energy-diagonal"

final class SpectralDiagonalFeature private (
    val times: Vector[Double],
    val kind: SpectralDiagonalKind
):
  def apply[V, I <: IndexedGraph[V]](
      spectrum: VertexSpectrum[V, I]
  ): Either[GraphSimilarityError, VertexFeatureSet[V, I]] =
    val rows = spectrum.index.vertexCount
    val cols = times.length
    val out = Matrix.newBuilder(rows, cols)
    var row = 0
    var error = Option.empty[GraphSimilarityError]
    while row < rows && error.isEmpty do
      var timeIndex = 0
      while timeIndex < cols && error.isEmpty do
        val time = times(timeIndex)
        var component = 0
        var value = 0.0
        while component < spectrum.result.size do
          val eigenvalue = Math.max(spectrum.result.eigenvalues(component), 0.0)
          val coordinate = spectrum.result.eigenvectors(row, component)
          value +=
            Math.exp(-kind.exponentFactor * time * eigenvalue) * coordinate * coordinate
          component += 1
        if !value.isFinite then
          error = Some(
            GraphSimilarityError.NonFiniteFeature(row * cols + timeIndex, value)
          )
        else out(row, timeIndex) = value
        timeIndex += 1
      row += 1
    error match
      case Some(value) =>
        Left(value)
      case None =>
        Right(
          new VertexFeatureSet(
            spectrum.index,
            out.result(),
            GraphFeatureSpec(kind.label, times)
          )
        )

object SpectralDiagonalFeature:
  def from(
      times: Iterable[Double],
      kind: SpectralDiagonalKind = SpectralDiagonalKind.HeatKernel
  ): Either[GraphSimilarityError, SpectralDiagonalFeature] =
    val values = times.toVector
    if values.isEmpty then
      Left(
        GraphSimilarityError.InvalidParameter(
          "spectral time count",
          0.0,
          "must be positive"
        )
      )
    else
      values.find(value => !value.isFinite || value < 0.0) match
        case Some(value) =>
          Left(
            GraphSimilarityError.InvalidParameter(
              "spectral time",
              value,
              "must be finite and non-negative"
            )
          )
        case None =>
          Right(new SpectralDiagonalFeature(values, kind))

final case class LinearFeatureSimilarity[V]()
    extends GraphSimilarity[VertexFeatureSet[V, ? <: IndexedGraph[V]]]:
  val name: String = "spectral-feature-linear"
  val psdStatus: PsdStatus =
    PsdStatus.KnownPsd("inner product of explicit finite vertex feature vectors")

  def apply(
      left: VertexFeatureSet[V, ? <: IndexedGraph[V]],
      right: VertexFeatureSet[V, ? <: IndexedGraph[V]]
  ): Either[GraphSimilarityError, Double] =
    validateComparable(left, right).map: _ =>
      var total = 0.0
      var row = 0
      while row < left.values.rows do
        var col = 0
        while col < left.values.cols do
          total += left.values(row, col) * right.values(row, col)
          col += 1
        row += 1
      total

final case class RbfFeatureSimilarity[V] private (gamma: Double)
    extends GraphSimilarity[VertexFeatureSet[V, ? <: IndexedGraph[V]]]:
  val name: String = "spectral-feature-rbf"
  val psdStatus: PsdStatus =
    PsdStatus.KnownPsd("Gaussian RBF over an explicit Euclidean feature map")

  def apply(
      left: VertexFeatureSet[V, ? <: IndexedGraph[V]],
      right: VertexFeatureSet[V, ? <: IndexedGraph[V]]
  ): Either[GraphSimilarityError, Double] =
    validateComparable(left, right).map: _ =>
      var squared = 0.0
      var row = 0
      while row < left.values.rows do
        var col = 0
        while col < left.values.cols do
          val difference = left.values(row, col) - right.values(row, col)
          squared += difference * difference
          col += 1
        row += 1
      Math.exp(-gamma * squared)

object RbfFeatureSimilarity:
  def from[V](
      gamma: Double
  ): Either[GraphSimilarityError, RbfFeatureSimilarity[V]] =
    if !gamma.isFinite || gamma <= 0.0 then
      Left(
        GraphSimilarityError.InvalidParameter(
          "RBF gamma",
          gamma,
          "must be finite and positive"
        )
      )
    else Right(new RbfFeatureSimilarity(gamma))

final case class NegativeEuclideanFeatureSimilarity[V]()
    extends GraphSimilarity[VertexFeatureSet[V, ? <: IndexedGraph[V]]]:
  val name: String = "negative-spectral-feature-distance"
  val psdStatus: PsdStatus =
    PsdStatus.NotEstablished(
      "negative Euclidean distance is a similarity, not a declared PSD kernel"
    )

  def apply(
      left: VertexFeatureSet[V, ? <: IndexedGraph[V]],
      right: VertexFeatureSet[V, ? <: IndexedGraph[V]]
  ): Either[GraphSimilarityError, Double] =
    validateComparable(left, right).map: _ =>
      var squared = 0.0
      var row = 0
      while row < left.values.rows do
        var col = 0
        while col < left.values.cols do
          val difference = left.values(row, col) - right.values(row, col)
          squared += difference * difference
          col += 1
        row += 1
      -Math.sqrt(squared)

private def validateComparable[V](
    left: VertexFeatureSet[V, ? <: IndexedGraph[V]],
    right: VertexFeatureSet[V, ? <: IndexedGraph[V]]
): Either[GraphSimilarityError, Unit] =
  val leftLabels = labels(left.index)
  val rightLabels = labels(right.index)
  if !sameOrderedLabels(left.index, right.index) then
    Left(
      GraphSimilarityError.BasisMismatch(
        leftLabels,
        rightLabels
      )
    )
  else if left.spec != right.spec then
    Left(GraphSimilarityError.FeatureMismatch(left.spec, right.spec))
  else Right(())

private def labels[V](index: IndexedGraph[V]): Vector[V] =
  index.vertices.map(index.label).toVector

private def sameOrderedLabels[V](
    left: IndexedGraph[V],
    right: IndexedGraph[V]
): Boolean =
  if left.vertexCount != right.vertexCount then false
  else
    val leftVertices = left.vertices
    val rightVertices = right.vertices
    var same = true
    while leftVertices.hasNext && rightVertices.hasNext && same do
      val leftLabel = left.label(leftVertices.next())
      val rightLabel = right.label(rightVertices.next())
      same = left.vertexHash.eqv(leftLabel, rightLabel) &&
        right.vertexHash.eqv(leftLabel, rightLabel)
    same
