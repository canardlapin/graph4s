package graph4s.data

import cats.Eq
import graph4s.Graph

final case class TopologyMismatch[V](
    expected: Graph[V],
    actual: Graph[V]
)

final class WeightedGraph[V, +W] private (
    val topology: Graph[V],
    val weights: EdgeField[V, W]
)

object WeightedGraph:
  def from[V, W](weights: EdgeField[V, W]): WeightedGraph[V, W] =
    new WeightedGraph(weights.topology, weights)

  def apply[V, W](
      topology: Graph[V],
      weights: EdgeField[V, W]
  ): Either[TopologyMismatch[V], WeightedGraph[V, W]] =
    if Eq[Graph[V]].eqv(topology, weights.topology) then Right(new WeightedGraph(topology, weights))
    else Left(TopologyMismatch(topology, weights.topology))
