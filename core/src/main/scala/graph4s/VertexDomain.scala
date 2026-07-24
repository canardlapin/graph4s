package graph4s

import cats.Hash

/** The finite vertex domain shared by persistent graphs and digraphs. */
trait VertexDomain[V]:
  def vertices: VertexSet[V]
  def containsVertex(vertex: V): Boolean
  def vertexHash: Hash[V]
