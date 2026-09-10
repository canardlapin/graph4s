package graph4s.indexed

/** A stable, densely ordered snapshot of a finite vertex domain.
  *
  * `Vertex` remains path-dependent: a coordinate from one snapshot cannot be used with another
  * snapshot even when both snapshots contain the same labels.
  */
trait IndexedVertexDomain[V]:
  type Vertex

  def vertexCount: Int
  def vertex(label: V): Option[Vertex]
  def ordinal(vertex: Vertex): Int
  def vertexAt(ordinal: Int): Option[Vertex]
  def label(vertex: Vertex): V
  def vertices: Iterator[Vertex]
