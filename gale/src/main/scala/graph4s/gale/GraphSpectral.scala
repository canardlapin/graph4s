package graph4s.gale

import scala.collection.mutable

import cats.data.NonEmptyVector
import gale.linalg.{DMat, DVec, Matrix}
import gale.spectral.{Eigen, EigenDecomposition, EigenOrder, EigenSelection}
import graph4s.data.IndexedEdgeField
import graph4s.indexed.IndexedGraph

object GraphSpectral:
  def spectrum[V, E, I <: IndexedGraph[V]](
      weights: IndexedEdgeField[V, E, I],
      rank: Int,
      operator: SpectralLaplacian = SpectralLaplacian.Combinatorial,
      spectrum: SpectralEnd = SpectralEnd.Smallest
  )(using
      weight: NonNegativeAdjacencyWeight[E]
  ): Either[GraphLinalgError[V], VertexSpectrum[V, I]] =
    prepare(weights, operator).flatMap: prepared =>
      decompose(prepared.operator, rank, spectrum).map: result =>
        new VertexSpectrum(
          weights.index,
          result,
          operator,
          prepared.support,
          prepared.expectedNullity
        )

  def embedding[V, E, I <: IndexedGraph[V]](
      weights: IndexedEdgeField[V, E, I],
      dimensions: Int,
      operator: SpectralLaplacian = SpectralLaplacian.Combinatorial,
      eigenvectors: EmbeddingEigenvectors = EmbeddingEigenvectors.DropExpectedNullspace
  )(using
      weight: NonNegativeAdjacencyWeight[E]
  ): Either[GraphLinalgError[V], SpectralEmbedding[V, I]] =
    if dimensions <= 0 then
      Left(
        GraphLinalgError.InvalidEmbeddingDimensions(
          dimensions,
          weights.index.vertexCount,
          0
        )
      )
    else
      prepare(weights, operator).flatMap: prepared =>
        val dropped =
          eigenvectors match
            case EmbeddingEigenvectors.IncludeSmallest       => 0
            case EmbeddingEigenvectors.DropExpectedNullspace => prepared.expectedNullity
        val requested = dimensions.toLong + dropped.toLong
        if requested > weights.index.vertexCount.toLong then
          Left(
            GraphLinalgError.InvalidEmbeddingDimensions(
              dimensions,
              weights.index.vertexCount,
              dropped
            )
          )
        else
          decompose(prepared.operator, requested.toInt, SpectralEnd.Smallest).map: result =>
            new SpectralEmbedding(
              weights.index,
              sliceVector(result.eigenvalues, dropped, dimensions),
              sliceColumns(result.eigenvectors, dropped, dimensions),
              sliceVector(result.diagnostics.residuals, dropped, dimensions),
              operator,
              prepared.support,
              dropped
            )

  private final case class Prepared[V, I <: IndexedGraph[V]](
      operator: VertexOperator[V, I],
      support: WeightedSupport[V, I],
      expectedNullity: Int
  )

  private def prepare[V, E, I <: IndexedGraph[V]](
      weights: IndexedEdgeField[V, E, I],
      kind: SpectralLaplacian
  )(using
      weight: NonNegativeAdjacencyWeight[E]
  ): Either[GraphLinalgError[V], Prepared[V, I]] =
    operatorFor(weights, kind).flatMap: operator =>
      supportFor(weights).map: support =>
        val expectedNullity =
          kind match
            case SpectralLaplacian.SymmetricNormalized(
                  ZeroStrengthPolicy.IdentityOnZeroStrength
                ) =>
              support.components.length - support.zeroStrengthVertices.length
            case _ =>
              support.components.length
        Prepared(operator, support, expectedNullity)

  private def operatorFor[V, E, I <: IndexedGraph[V]](
      weights: IndexedEdgeField[V, E, I],
      kind: SpectralLaplacian
  )(using
      weight: NonNegativeAdjacencyWeight[E]
  ): Either[GraphLinalgError[V], VertexOperator[V, I]] =
    kind match
      case SpectralLaplacian.Combinatorial =>
        GraphOperators.combinatorialLaplacian(weights)
      case SpectralLaplacian.SymmetricNormalized(policy) =>
        GraphOperators.normalizedLaplacian(
          weights,
          NormalizedLaplacian.Symmetric,
          policy
        )

  private def supportFor[V, E, I <: IndexedGraph[V]](
      weights: IndexedEdgeField[V, E, I]
  )(using
      weight: NonNegativeAdjacencyWeight[E]
  ): Either[GraphLinalgError[V], WeightedSupport[V, I]] =
    val index = weights.index
    val positiveDegree = Array.fill(index.vertexCount)(0)
    val parent = Array.tabulate(index.vertexCount)(identity)
    val rank = Array.fill(index.vertexCount)(0)
    var zeroEdges = 0
    var error = Option.empty[GraphLinalgError[V]]

    def rootOf(vertex: Int): Int =
      var root = vertex
      while parent(root) != root do root = parent(root)
      var current = vertex
      while parent(current) != current do
        val next = parent(current)
        parent(current) = root
        current = next
      root

    def union(left: Int, right: Int): Unit =
      val leftRoot = rootOf(left)
      val rightRoot = rootOf(right)
      if leftRoot != rightRoot then
        if rank(leftRoot) < rank(rightRoot) then parent(leftRoot) = rightRoot
        else if rank(leftRoot) > rank(rightRoot) then parent(rightRoot) = leftRoot
        else
          parent(rightRoot) = leftRoot
          rank(leftRoot) += 1

    val edges = index.edges
    while edges.hasNext && error.isEmpty do
      val edge = edges.next()
      val ordinal = index.edgeOrdinal(edge)
      val value = weight.weight(weights.unsafeValueAt(ordinal))
      val left = index.first(edge)
      val right = index.second(edge)
      if !value.isFinite || value < 0.0 then
        error = Some(
          GraphLinalgError.InvalidWeight(
            ordinal,
            index.label(left),
            index.label(right),
            value,
            WeightRequirement.NonNegative
          )
        )
      else if value == 0.0 then zeroEdges += 1
      else
        val leftOrdinal = index.ordinal(left)
        val rightOrdinal = index.ordinal(right)
        positiveDegree(leftOrdinal) += 1
        positiveDegree(rightOrdinal) += 1
        union(leftOrdinal, rightOrdinal)

    error match
      case Some(value) =>
        Left(value)
      case None =>
        val labelsByRoot = mutable.LinkedHashMap.empty[Int, NonEmptyVector[V]]
        val vertices = index.vertices
        while vertices.hasNext do
          val vertex = vertices.next()
          val root = rootOf(index.ordinal(vertex))
          val label = index.label(vertex)
          labelsByRoot.get(root) match
            case Some(labels) => labelsByRoot.update(root, labels.append(label))
            case None         => labelsByRoot.update(root, NonEmptyVector.one(label))
        val components =
          labelsByRoot.valuesIterator.map(Component(_)).toVector
        val zeroStrength =
          index.vertices
            .filter(vertex => positiveDegree(index.ordinal(vertex)) == 0)
            .map(index.label)
            .toVector
        Right(
          new WeightedSupport[V, I](
            index,
            components,
            zeroEdges,
            zeroStrength
          )
        )

  private def decompose[V, I <: IndexedGraph[V]](
      operator: VertexOperator[V, I],
      rank: Int,
      spectrum: SpectralEnd
  ): Either[GraphLinalgError[Nothing], EigenDecomposition] =
    if rank <= 0 || rank > operator.index.vertexCount then
      Left(GraphLinalgError.InvalidSpectrumRank(rank, operator.index.vertexCount))
    else
      val selection =
        if rank == operator.index.vertexCount then EigenSelection.All
        else
          val order =
            spectrum match
              case SpectralEnd.Smallest => EigenOrder.SmallestAlgebraic
              case SpectralEnd.Largest  => EigenOrder.LargestAlgebraic
          EigenSelection.Count(rank, order)
      // Gale's current single-vector Lanczos path does not recover repeated
      // eigenvalue multiplicity. Graph embeddings need the complete repeated
      // eigenspace, so select from Gale's dense symmetric decomposition.
      Eigen
        .eigSymmetric(operator.matrix.toDense(), selection)
        .left
        .map(GraphLinalgError.EigenFailure.apply)
        .flatMap(_.requireConverged.left.map(GraphLinalgError.EigenFailure.apply))

  private def sliceVector(values: DVec, start: Int, count: Int): DVec =
    values.slice(start, start + count).copy

  private def sliceColumns(matrix: DMat, start: Int, count: Int): DMat =
    val out = Matrix.newBuilder(matrix.rows, count)
    var row = 0
    while row < matrix.rows do
      var col = 0
      while col < count do
        out(row, col) = matrix(row, start + col)
        col += 1
      row += 1
    out.result()

extension [V, E, I <: IndexedGraph[V]](weights: IndexedEdgeField[V, E, I])
  def vertexSpectrum(
      rank: Int,
      operator: SpectralLaplacian = SpectralLaplacian.Combinatorial,
      spectrum: SpectralEnd = SpectralEnd.Smallest
  )(using
      weight: NonNegativeAdjacencyWeight[E]
  ): Either[GraphLinalgError[V], VertexSpectrum[V, I]] =
    GraphSpectral.spectrum(weights, rank, operator, spectrum)

  def spectralEmbedding(
      dimensions: Int,
      operator: SpectralLaplacian = SpectralLaplacian.Combinatorial,
      eigenvectors: EmbeddingEigenvectors = EmbeddingEigenvectors.DropExpectedNullspace
  )(using
      weight: NonNegativeAdjacencyWeight[E]
  ): Either[GraphLinalgError[V], SpectralEmbedding[V, I]] =
    GraphSpectral.embedding(weights, dimensions, operator, eigenvectors)
