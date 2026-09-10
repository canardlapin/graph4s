package graph4s.data

import cats.Eq
import graph4s.{Digraph, Graph}
import graph4s.indexed.{IndexedDigraph, IndexedGraph}

final case class TopologyMismatch[V](
    expected: Graph[V],
    actual: Graph[V]
)

final case class DigraphTopologyMismatch[V](
    expected: Digraph[V],
    actual: Digraph[V]
)

final class WeightedGraph[V, +W] private (
    val topology: Graph[V],
    val weights: EdgeField[V, W]
):
  def indexedBy(
      index: IndexedGraph[V]
  ): Either[TopologyMismatch[V], IndexedEdgeField[V, W, index.type]] =
    IndexedEdgeField.from(index, weights)

object WeightedGraph:
  def from[V, W](weights: EdgeField[V, W]): WeightedGraph[V, W] =
    new WeightedGraph(weights.topology, weights)

  def apply[V, W](
      topology: Graph[V],
      weights: EdgeField[V, W]
  ): Either[TopologyMismatch[V], WeightedGraph[V, W]] =
    if Eq[Graph[V]].eqv(topology, weights.topology) then Right(new WeightedGraph(topology, weights))
    else Left(TopologyMismatch(topology, weights.topology))

final class WeightedDigraph[V, +W] private (
    val topology: Digraph[V],
    val weights: ArcField[V, W]
):
  def indexedBy(
      index: IndexedDigraph[V]
  ): Either[DigraphTopologyMismatch[V], IndexedArcField[V, W, index.type]] =
    IndexedArcField.from(index, weights)

object WeightedDigraph:
  def from[V, W](weights: ArcField[V, W]): WeightedDigraph[V, W] =
    new WeightedDigraph(weights.topology, weights)

  def apply[V, W](
      topology: Digraph[V],
      weights: ArcField[V, W]
  ): Either[DigraphTopologyMismatch[V], WeightedDigraph[V, W]] =
    if Eq[Digraph[V]].eqv(topology, weights.topology) then
      Right(new WeightedDigraph(topology, weights))
    else Left(DigraphTopologyMismatch(topology, weights.topology))
