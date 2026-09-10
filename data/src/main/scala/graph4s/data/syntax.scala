package graph4s.data

import graph4s.{InducedSubgraph, InducedSubgraphDigraph}

object syntax:
  extension [V](induced: InducedSubgraph[V])
    def restrict[A](
        field: VertexField[V, A]
    ): Either[VertexTopologyMismatch[V], VertexField[V, A]] =
      VertexField.restrict(induced, field)

    def restrict[A](
        field: EdgeField[V, A]
    ): Either[TopologyMismatch[V], EdgeField[V, A]] =
      EdgeField.restrict(induced, field)

  extension [V](induced: InducedSubgraphDigraph[V])
    def restrict[A](
        field: VertexField[V, A]
    ): Either[VertexTopologyMismatch[V], VertexField[V, A]] =
      VertexField.restrict(induced, field)

    def restrict[A](
        field: ArcField[V, A]
    ): Either[DigraphTopologyMismatch[V], ArcField[V, A]] =
      ArcField.restrict(induced, field)
