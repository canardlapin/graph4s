package graph4s

package object expr:
  def vertex[V](value: V): GraphExpr[V] =
    GraphExpr.vertex(value)

  def vertices[V](values: V*): GraphExpr[V] =
    GraphExpr.vertices(values*)

  object directed:
    def vertex[V](value: V): DigraphExpr[V] =
      DigraphExpr.vertex(value)

    def vertices[V](values: V*): DigraphExpr[V] =
      DigraphExpr.vertices(values*)
