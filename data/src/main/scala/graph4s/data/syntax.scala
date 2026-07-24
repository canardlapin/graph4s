package graph4s.data

import graph4s.InducedSubgraph

object syntax:
  extension [V](induced: InducedSubgraph[V])
    def restrict[A](
        field: VertexField[V, A]
    ): Either[TopologyMismatch[V], VertexField[V, A]] =
      VertexField.restrict(induced, field)

    def restrict[A](
        field: EdgeField[V, A]
    ): Either[TopologyMismatch[V], EdgeField[V, A]] =
      EdgeField.restrict(induced, field)
