package graph4s

import cats.data.NonEmptyChain

enum GraphBuildError[V]:
  case SelfLoop(vertex: V)
  case UnknownEndpoint(edge: Link[V], endpoint: V)

enum DigraphBuildError[V]:
  case SelfLoop(vertex: V)
  case UnknownEndpoint(arc: ArcInput[V], endpoint: V)

enum ConnectError[V]:
  case SelfLoop(vertex: V)
  case MissingEndpoints(vertices: NonEmptyChain[V])

enum MissingVertex[V]:
  case Vertex(vertex: V)
  case Vertices(vertices: NonEmptyChain[V])

enum ConnectivityError[V]:
  case EmptyGraph[V]() extends ConnectivityError[V]
  case Disconnected[V](components: Vector[VertexSet[V]]) extends ConnectivityError[V]
