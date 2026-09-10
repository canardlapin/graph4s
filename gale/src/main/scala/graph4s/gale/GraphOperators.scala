package graph4s.gale

import scala.annotation.targetName

import gale.linalg.Vec
import gale.sparse.{COOBuilder, Sparse}
import graph4s.data.{IndexedArcField, IndexedEdgeField}
import graph4s.indexed.{IndexedDigraph, IndexedGraph, IndexedVertexDomain}

object GraphOperators:
  def topologyAdjacency[V](
      graph: IndexedGraph[V]
  ): VertexOperator[V, graph.type] =
    val entries = Sparse.coo(graph.vertexCount, graph.vertexCount)
    graph.edges.foreach: edge =>
      val leftOrdinal = graph.ordinal(graph.first(edge))
      val rightOrdinal = graph.ordinal(graph.second(edge))
      entries.add(leftOrdinal, rightOrdinal, 1.0)
      entries.add(rightOrdinal, leftOrdinal, 1.0)
    vertexOperator(graph, entries)

  @targetName("digraphTopologyAdjacency")
  def topologyAdjacency[V](
      graph: IndexedDigraph[V]
  ): VertexOperator[V, graph.type] =
    val entries = Sparse.coo(graph.vertexCount, graph.vertexCount)
    graph.arcs.foreach: arc =>
      entries.add(graph.ordinal(graph.source(arc)), graph.ordinal(graph.target(arc)), 1.0)
    vertexOperator(graph, entries)

  def weightedAdjacency[V, E, I <: IndexedGraph[V]](
      weights: IndexedEdgeField[V, E, I]
  )(using weight: AdjacencyWeight[E]): Either[GraphLinalgError[V], VertexOperator[V, I]] =
    validatedWeights(weights, WeightRequirement.Finite).map: values =>
      val graph = weights.index
      val entries = Sparse.coo(graph.vertexCount, graph.vertexCount)
      graph.edges.foreach: edge =>
        val ordinal = graph.edgeOrdinal(edge)
        val value = values(ordinal)
        val left = graph.ordinal(graph.first(edge))
        val right = graph.ordinal(graph.second(edge))
        entries.add(left, right, value)
        entries.add(right, left, value)
      vertexOperator(graph, entries)

  @targetName("digraphWeightedAdjacency")
  def weightedAdjacency[V, E, I <: IndexedDigraph[V]](
      weights: IndexedArcField[V, E, I]
  )(using weight: AdjacencyWeight[E]): Either[GraphLinalgError[V], VertexOperator[V, I]] =
    validatedWeights(weights, WeightRequirement.Finite).map: values =>
      val graph = weights.index
      val entries = Sparse.coo(graph.vertexCount, graph.vertexCount)
      graph.arcs.foreach: arc =>
        entries.add(
          graph.ordinal(graph.source(arc)),
          graph.ordinal(graph.target(arc)),
          values(graph.arcOrdinal(arc))
        )
      vertexOperator(graph, entries)

  def degree[V](graph: IndexedGraph[V]): VertexSignal[V, graph.type] =
    val values = Vec.tabulate(graph.vertexCount): ordinal =>
      graph.vertexAt(ordinal).fold(0.0)(vertex => graph.degree(vertex).toDouble)
    new VertexSignal[V, graph.type](graph, values)

  def outDegree[V](graph: IndexedDigraph[V]): VertexSignal[V, graph.type] =
    val values = Vec.tabulate(graph.vertexCount): ordinal =>
      graph.vertexAt(ordinal).fold(0.0)(vertex => graph.outDegree(vertex).toDouble)
    new VertexSignal[V, graph.type](graph, values)

  def inDegree[V](graph: IndexedDigraph[V]): VertexSignal[V, graph.type] =
    val values = Vec.tabulate(graph.vertexCount): ordinal =>
      graph.vertexAt(ordinal).fold(0.0)(vertex => graph.inDegree(vertex).toDouble)
    new VertexSignal[V, graph.type](graph, values)

  def strength[V, E, I <: IndexedGraph[V]](
      weights: IndexedEdgeField[V, E, I]
  )(using weight: AdjacencyWeight[E]): Either[GraphLinalgError[V], VertexSignal[V, I]] =
    validatedWeights(weights, WeightRequirement.Finite).map: values =>
      val strengths = undirectedStrength(weights.index, values)
      new VertexSignal[V, I](
        weights.index,
        Vec.tabulate(strengths.length)(strengths.apply)
      )

  def outStrength[V, E, I <: IndexedDigraph[V]](
      weights: IndexedArcField[V, E, I]
  )(using weight: AdjacencyWeight[E]): Either[GraphLinalgError[V], VertexSignal[V, I]] =
    validatedWeights(weights, WeightRequirement.Finite).map: values =>
      val graph = weights.index
      val strengths = Array.ofDim[Double](graph.vertexCount)
      graph.arcs.foreach: arc =>
        strengths(graph.ordinal(graph.source(arc))) += values(graph.arcOrdinal(arc))
      new VertexSignal[V, I](graph, Vec.tabulate(strengths.length)(strengths.apply))

  def inStrength[V, E, I <: IndexedDigraph[V]](
      weights: IndexedArcField[V, E, I]
  )(using weight: AdjacencyWeight[E]): Either[GraphLinalgError[V], VertexSignal[V, I]] =
    validatedWeights(weights, WeightRequirement.Finite).map: values =>
      val graph = weights.index
      val strengths = Array.ofDim[Double](graph.vertexCount)
      graph.arcs.foreach: arc =>
        strengths(graph.ordinal(graph.target(arc))) += values(graph.arcOrdinal(arc))
      new VertexSignal[V, I](graph, Vec.tabulate(strengths.length)(strengths.apply))

  def incidence[V](graph: IndexedGraph[V]): GraphIncidenceOperator[V, graph.type] =
    val entries = Sparse.coo(graph.vertexCount, graph.edgeCount)
    graph.edges.foreach: edge =>
      val column = graph.edgeOrdinal(edge)
      entries.add(graph.ordinal(graph.first(edge)), column, -1.0)
      entries.add(graph.ordinal(graph.second(edge)), column, 1.0)
    new GraphIncidenceOperator[V, graph.type](graph, entries.pruneZeros.toCSR())

  @targetName("digraphIncidence")
  def incidence[V](graph: IndexedDigraph[V]): DigraphIncidenceOperator[V, graph.type] =
    val entries = Sparse.coo(graph.vertexCount, graph.arcCount)
    graph.arcs.foreach: arc =>
      val column = graph.arcOrdinal(arc)
      entries.add(graph.ordinal(graph.source(arc)), column, -1.0)
      entries.add(graph.ordinal(graph.target(arc)), column, 1.0)
    new DigraphIncidenceOperator[V, graph.type](graph, entries.pruneZeros.toCSR())

  def combinatorialLaplacian[V, E, I <: IndexedGraph[V]](
      weights: IndexedEdgeField[V, E, I]
  )(using
      weight: NonNegativeAdjacencyWeight[E]
  ): Either[GraphLinalgError[V], VertexOperator[V, I]] =
    validatedWeights(weights, WeightRequirement.NonNegative).map: values =>
      val graph = weights.index
      val strengths = undirectedStrength(graph, values)
      val entries = Sparse.coo(graph.vertexCount, graph.vertexCount)
      var vertex = 0
      while vertex < graph.vertexCount do
        entries.add(vertex, vertex, strengths(vertex))
        vertex += 1
      graph.edges.foreach: edge =>
        val value = -values(graph.edgeOrdinal(edge))
        val left = graph.ordinal(graph.first(edge))
        val right = graph.ordinal(graph.second(edge))
        entries.add(left, right, value)
        entries.add(right, left, value)
      vertexOperator(graph, entries)

  def normalizedLaplacian[V, E, I <: IndexedGraph[V]](
      weights: IndexedEdgeField[V, E, I],
      kind: NormalizedLaplacian,
      zeroStrengthPolicy: ZeroStrengthPolicy
  )(using
      weight: NonNegativeAdjacencyWeight[E]
  ): Either[GraphLinalgError[V], VertexOperator[V, I]] =
    validatedWeights(weights, WeightRequirement.NonNegative).flatMap: values =>
      val graph = weights.index
      val strengths = undirectedStrength(graph, values)
      firstZeroStrength(graph, strengths, zeroStrengthPolicy) match
        case Some(error) =>
          Left(error)
        case None =>
          val entries = Sparse.coo(graph.vertexCount, graph.vertexCount)
          var vertex = 0
          while vertex < graph.vertexCount do
            if strengths(vertex) > 0.0 ||
              zeroStrengthPolicy == ZeroStrengthPolicy.IdentityOnZeroStrength
            then
              entries.add(vertex, vertex, 1.0)
              ()
            vertex += 1

          graph.edges.foreach: edge =>
            val ordinal = graph.edgeOrdinal(edge)
            val edgeWeight = values(ordinal)
            if edgeWeight != 0.0 then
              val left = graph.ordinal(graph.first(edge))
              val right = graph.ordinal(graph.second(edge))
              kind match
                case NormalizedLaplacian.Symmetric =>
                  val value = -edgeWeight / Math.sqrt(strengths(left) * strengths(right))
                  entries.add(left, right, value)
                  entries.add(right, left, value)
                  ()
                case NormalizedLaplacian.RandomWalk =>
                  entries.add(left, right, -edgeWeight / strengths(left))
                  entries.add(right, left, -edgeWeight / strengths(right))
                  ()
          Right(vertexOperator(graph, entries))

  private def validatedWeights[V, E, I <: IndexedGraph[V]](
      weights: IndexedEdgeField[V, E, I],
      requirement: WeightRequirement
  )(using weight: AdjacencyWeight[E]): Either[GraphLinalgError[V], Array[Double]] =
    val graph = weights.index
    val values = new Array[Double](weights.size)
    var error = Option.empty[GraphLinalgError[V]]
    val edges = graph.edges
    while edges.hasNext && error.isEmpty do
      val edge = edges.next()
      val ordinal = graph.edgeOrdinal(edge)
      val value = weight.weight(weights.unsafeValueAt(ordinal))
      val left = graph.first(edge)
      val right = graph.second(edge)
      if invalid(value, requirement) then
        error = Some(
          GraphLinalgError.InvalidWeight(
            ordinal,
            graph.label(left),
            graph.label(right),
            value,
            requirement
          )
        )
      else values(ordinal) = value
    error.toLeft(values)

  private def validatedWeights[V, E, I <: IndexedDigraph[V]](
      weights: IndexedArcField[V, E, I],
      requirement: WeightRequirement
  )(using weight: AdjacencyWeight[E]): Either[GraphLinalgError[V], Array[Double]] =
    val graph = weights.index
    val values = new Array[Double](weights.size)
    var error = Option.empty[GraphLinalgError[V]]
    val arcs = graph.arcs
    while arcs.hasNext && error.isEmpty do
      val arc = arcs.next()
      val ordinal = graph.arcOrdinal(arc)
      val value = weight.weight(weights.unsafeValueAt(ordinal))
      val source = graph.source(arc)
      val target = graph.target(arc)
      if invalid(value, requirement) then
        error = Some(
          GraphLinalgError.InvalidWeight(
            ordinal,
            graph.label(source),
            graph.label(target),
            value,
            requirement
          )
        )
      else values(ordinal) = value
    error.toLeft(values)

  private def invalid(value: Double, requirement: WeightRequirement): Boolean =
    !value.isFinite ||
      (requirement == WeightRequirement.NonNegative && value < 0.0) ||
      (requirement == WeightRequirement.StrictlyPositive && value <= 0.0)

  private def undirectedStrength[V](
      graph: IndexedGraph[V],
      weights: Array[Double]
  ): Array[Double] =
    val strengths = Array.ofDim[Double](graph.vertexCount)
    graph.edges.foreach: edge =>
      val value = weights(graph.edgeOrdinal(edge))
      strengths(graph.ordinal(graph.first(edge))) += value
      strengths(graph.ordinal(graph.second(edge))) += value
    strengths

  private def firstZeroStrength[V](
      graph: IndexedGraph[V],
      strengths: Array[Double],
      policy: ZeroStrengthPolicy
  ): Option[GraphLinalgError[V]] =
    if policy != ZeroStrengthPolicy.Error then None
    else
      var ordinal = 0
      var error = Option.empty[GraphLinalgError[V]]
      val vertices = graph.vertices
      while ordinal < graph.vertexCount && error.isEmpty do
        val vertex = vertices.next()
        if strengths(ordinal) == 0.0 then
          error = Some(GraphLinalgError.ZeroStrengthVertex(graph.label(vertex)))
        ordinal += 1
      error

  private def vertexOperator[V, I <: IndexedVertexDomain[V]](
      graph: I,
      entries: COOBuilder
  ): VertexOperator[V, I] =
    new VertexOperator(graph, entries.pruneZeros.toCSR())

extension [V](graph: IndexedGraph[V])
  def topologyAdjacency: VertexOperator[V, graph.type] =
    GraphOperators.topologyAdjacency(graph)

  def degreeSignal: VertexSignal[V, graph.type] =
    GraphOperators.degree(graph)

  def incidence: GraphIncidenceOperator[V, graph.type] =
    GraphOperators.incidence(graph)

extension [V](graph: IndexedDigraph[V])
  @targetName("digraphTopologyAdjacencyExtension")
  def topologyAdjacency: VertexOperator[V, graph.type] =
    GraphOperators.topologyAdjacency(graph)

  def outDegreeSignal: VertexSignal[V, graph.type] =
    GraphOperators.outDegree(graph)

  def inDegreeSignal: VertexSignal[V, graph.type] =
    GraphOperators.inDegree(graph)

  @targetName("digraphIncidenceExtension")
  def incidence: DigraphIncidenceOperator[V, graph.type] =
    GraphOperators.incidence(graph)

extension [V, E, I <: IndexedGraph[V]](weights: IndexedEdgeField[V, E, I])
  def weightedAdjacency(using
      weight: AdjacencyWeight[E]
  ): Either[GraphLinalgError[V], VertexOperator[V, I]] =
    GraphOperators.weightedAdjacency(weights)

  def strength(using
      weight: AdjacencyWeight[E]
  ): Either[GraphLinalgError[V], VertexSignal[V, I]] =
    GraphOperators.strength(weights)

  def combinatorialLaplacian(using
      weight: NonNegativeAdjacencyWeight[E]
  ): Either[GraphLinalgError[V], VertexOperator[V, I]] =
    GraphOperators.combinatorialLaplacian(weights)

  def normalizedLaplacian(
      kind: NormalizedLaplacian,
      zeroStrengthPolicy: ZeroStrengthPolicy
  )(using
      weight: NonNegativeAdjacencyWeight[E]
  ): Either[GraphLinalgError[V], VertexOperator[V, I]] =
    GraphOperators.normalizedLaplacian(weights, kind, zeroStrengthPolicy)

extension [V, E, I <: IndexedDigraph[V]](weights: IndexedArcField[V, E, I])
  @targetName("digraphWeightedAdjacencyExtension")
  def weightedAdjacency(using
      weight: AdjacencyWeight[E]
  ): Either[GraphLinalgError[V], VertexOperator[V, I]] =
    GraphOperators.weightedAdjacency(weights)

  def outStrength(using
      weight: AdjacencyWeight[E]
  ): Either[GraphLinalgError[V], VertexSignal[V, I]] =
    GraphOperators.outStrength(weights)

  def inStrength(using
      weight: AdjacencyWeight[E]
  ): Either[GraphLinalgError[V], VertexSignal[V, I]] =
    GraphOperators.inStrength(weights)
