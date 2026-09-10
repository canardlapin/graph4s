package graph4s.gale

import cats.data.NonEmptyVector
import gale.linalg.{DMat, DVec, DoubleLinearOperator}
import gale.sparse.CSR
import gale.spectral.EigenDecomposition
import graph4s.indexed.{IndexedDigraph, IndexedGraph, IndexedVertexDomain}

final class VertexOperator[V, I <: IndexedVertexDomain[V]] private[gale] (
    val index: I,
    val matrix: CSR
):
  require(
    matrix.rows == index.vertexCount && matrix.cols == index.vertexCount,
    "vertex operator must be square in its indexed vertex domain"
  )

  def map: DoubleLinearOperator =
    matrix

final class VertexSignal[V, I <: IndexedVertexDomain[V]] private[gale] (
    val index: I,
    val values: DVec
):
  require(values.length == index.vertexCount, "vertex signal length must match vertex count")

  def apply(vertex: index.Vertex): Double =
    values(index.ordinal(vertex))

  def valueOf(label: V): Option[Double] =
    index.vertex(label).map(apply)

final class GraphIncidenceOperator[V, I <: IndexedGraph[V]] private[gale] (
    val index: I,
    val matrix: CSR
):
  require(matrix.rows == index.vertexCount, "incidence rows must match vertex count")
  require(matrix.cols == index.edgeCount, "incidence columns must match edge count")

  def map: DoubleLinearOperator =
    matrix

final class DigraphIncidenceOperator[V, I <: IndexedDigraph[V]] private[gale] (
    val index: I,
    val matrix: CSR
):
  require(matrix.rows == index.vertexCount, "incidence rows must match vertex count")
  require(matrix.cols == index.arcCount, "incidence columns must match arc count")

  def map: DoubleLinearOperator =
    matrix

enum NormalizedLaplacian:
  case Symmetric
  case RandomWalk

enum ZeroStrengthPolicy:
  case KeepZeroRow
  case IdentityOnZeroStrength
  case Error

enum SpectralLaplacian:
  case Combinatorial
  case SymmetricNormalized(zeroStrengthPolicy: ZeroStrengthPolicy)

enum EmbeddingEigenvectors:
  case IncludeSmallest
  case DropExpectedNullspace

/** Which algebraic end supplies a count-limited spectrum.
  *
  * Gale fixes the returned layout to ascending algebraic order for either choice.
  */
enum SpectralEnd:
  case Smallest
  case Largest

final case class Component[V] private[gale] (vertices: NonEmptyVector[V])

final class WeightedSupport[V, I <: IndexedGraph[V]] private[gale] (
    val index: I,
    val components: Vector[Component[V]],
    val zeroWeightEdgeCount: Int,
    val zeroStrengthVertices: Vector[V]
):
  require(zeroWeightEdgeCount >= 0, "zero-weight edge count must be non-negative")

final class VertexSpectrum[V, I <: IndexedGraph[V]] private[gale] (
    val index: I,
    val result: EigenDecomposition,
    val operator: SpectralLaplacian,
    val support: WeightedSupport[V, I],
    val expectedZeroEigenvalueMultiplicity: Int
):
  require(result.eigenvectors.rows == index.vertexCount, "spectrum rows must use the index")
  require(expectedZeroEigenvalueMultiplicity >= 0, "expected nullity must be non-negative")

final class SpectralEmbedding[V, I <: IndexedGraph[V]] private[gale] (
    val index: I,
    val eigenvalues: DVec,
    val coordinates: DMat,
    val residualNorms: DVec,
    val operator: SpectralLaplacian,
    val support: WeightedSupport[V, I],
    val droppedEigenvectors: Int
):
  require(coordinates.rows == index.vertexCount, "embedding rows must use the index")
  require(coordinates.cols == eigenvalues.length, "embedding columns must match eigenvalues")
  require(residualNorms.length == eigenvalues.length, "embedding residuals must match eigenvalues")
  require(droppedEigenvectors >= 0, "dropped eigenvector count must be non-negative")
