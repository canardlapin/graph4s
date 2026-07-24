package graph4s.laws

import cats.{Eq, Order}
import graph4s.{Digraph, Graph}
import graph4s.indexed.{IndexedDigraph, IndexedGraph, VertexOrder}
import org.scalacheck.Gen
import org.scalacheck.Prop.forAll
import org.typelevel.discipline.Laws

trait GraphLaws extends Laws:
  def structural[V](
      graphs: Gen[Graph[V]]
  )(using Eq[Graph[V]], Order[V]): RuleSet =
    new DefaultRuleSet(
      name = "graph.structural",
      parent = None,
      "neighbors are vertices" -> forAll(graphs) { graph =>
        graph.vertices.iterator.forall(vertex =>
          graph
            .neighborsOf(vertex)
            .exists(_.iterator.forall(graph.containsVertex))
        )
      },
      "loopless" -> forAll(graphs) { graph =>
        graph.vertices.iterator.forall(vertex => !graph.containsEdge(vertex, vertex))
      },
      "symmetric adjacency" -> forAll(graphs) { graph =>
        graph.vertices.iterator.forall(vertex =>
          graph
            .neighborsOf(vertex)
            .exists(_.iterator.forall(neighbor => graph.containsEdge(neighbor, vertex)))
        )
      },
      "handshake lemma" -> forAll(graphs) { graph =>
        graph.vertices.iterator.flatMap(graph.degreeOf).map(_.toLong).sum ==
          2L * graph.edgeCount
      },
      "complement involution" -> forAll(graphs) { graph =>
        Eq[Graph[V]].eqv(graph.complement.complement, graph)
      },
      "indexed round-trip" -> forAll(graphs) { graph =>
        IndexedGraph
          .from(graph, VertexOrder.by(Order[V]))
          .toOption
          .exists(indexed => Eq[Graph[V]].eqv(indexed.toGraph, graph))
      }
    )

  def digraphStructural[V](
      graphs: Gen[Digraph[V]]
  )(using Eq[Digraph[V]], Order[V]): RuleSet =
    new DefaultRuleSet(
      name = "digraph.structural",
      parent = None,
      "successors and predecessors are vertices" -> forAll(graphs) { graph =>
        graph.vertices.iterator.forall { vertex =>
          graph
            .successorsOf(vertex)
            .exists(_.iterator.forall(graph.containsVertex)) &&
          graph
            .predecessorsOf(vertex)
            .exists(_.iterator.forall(graph.containsVertex))
        }
      },
      "loopless" -> forAll(graphs) { graph =>
        graph.vertices.iterator.forall(vertex => !graph.containsArc(vertex, vertex))
      },
      "successor and predecessor indexes agree" -> forAll(graphs) { graph =>
        graph.arcs.iterator.forall { arc =>
          graph.successorsOf(arc.source).exists(_.contains(arc.target)) &&
          graph.predecessorsOf(arc.target).exists(_.contains(arc.source))
        }
      },
      "degree sums equal arc count" -> forAll(graphs) { graph =>
        val out =
          graph.vertices.iterator.flatMap(graph.outDegreeOf).map(_.toLong).sum
        val in =
          graph.vertices.iterator.flatMap(graph.inDegreeOf).map(_.toLong).sum
        out == graph.arcCount && in == graph.arcCount
      },
      "transpose involution" -> forAll(graphs) { graph =>
        Eq[Digraph[V]].eqv(graph.transpose.transpose, graph)
      },
      "indexed round-trip" -> forAll(graphs) { graph =>
        IndexedDigraph
          .from(graph, VertexOrder.by(Order[V]))
          .toOption
          .exists(indexed => Eq[Digraph[V]].eqv(indexed.toDigraph, graph))
      }
    )

object GraphLaws extends GraphLaws
