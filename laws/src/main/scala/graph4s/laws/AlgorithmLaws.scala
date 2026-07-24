package graph4s.laws

import cats.Order
import graph4s.{Digraph, Graph}
import graph4s.algorithms.{DigraphAlgorithms, GraphAlgorithms}
import org.scalacheck.Gen
import org.scalacheck.Prop.forAll
import org.typelevel.discipline.Laws
import scala.collection.mutable

trait AlgorithmLaws extends Laws:
  def referenceConformance(
      graphs: Gen[Graph[Int]],
      digraphs: Gen[Digraph[Int]]
  )(using Order[Int]): RuleSet =
    new DefaultRuleSet(
      name = "algorithms.reference-conformance",
      parent = None,
      "DFS preorder and parent match an independent recursive DFS" ->
        forAll(graphs) { graph =>
          if graph.vertexCount == 0 then true
          else
            val expected = referenceDfs(graph, 0)
            GraphAlgorithms
              .dfs(graph, 0)
              .toOption
              .exists { actual =>
                actual.preorder == expected._1 &&
                actual.parent.iterator.toMap == expected._2
              }
        },
      "BFS distances match an independent queue traversal" ->
        forAll(graphs) { graph =>
          if graph.vertexCount == 0 then true
          else
            GraphAlgorithms
              .bfs(graph, 0)
              .toOption
              .exists(_.distance.iterator.toMap == referenceDistances(graph, 0))
        },
      "topological results or cycle witnesses satisfy the source digraph" ->
        forAll(digraphs) { graph =>
          DigraphAlgorithms.topologicalSort(graph) match
            case Right(order) =>
              val position = order.zipWithIndex.toMap
              graph.arcs.iterator.forall { arc =>
                position(arc.source) < position(arc.target)
              }
            case Left(cycle) =>
              val vertices = cycle.vertices.toVector
              val targets = vertices.drop(1) :+ cycle.vertices.head
              vertices
                .zip(targets)
                .forall((source, target) => graph.containsArc(source, target))
        }
    )

  private def referenceDfs(
      graph: Graph[Int],
      root: Int
  ): (Vector[Int], Map[Int, Option[Int]]) =
    val visited = mutable.HashSet.empty[Int]
    val preorder = Vector.newBuilder[Int]
    val parent = mutable.Map.empty[Int, Option[Int]]

    def visit(vertex: Int, discoverer: Option[Int]): Unit =
      if visited.add(vertex) then
        preorder += vertex
        parent.put(vertex, discoverer)
        graph
          .neighborsOf(vertex)
          .iterator
          .flatMap(_.iterator)
          .toVector
          .sorted
          .foreach(neighbor => visit(neighbor, Some(vertex)))

    visit(root, None)
    (preorder.result(), parent.toMap)

  private def referenceDistances(
      graph: Graph[Int],
      root: Int
  ): Map[Int, Int] =
    val distance = mutable.Map(root -> 0)
    val queue = mutable.ArrayDeque(root)
    while queue.nonEmpty do
      val current = queue.removeHead()
      graph
        .neighborsOf(current)
        .iterator
        .flatMap(_.iterator)
        .toVector
        .sorted
        .foreach { neighbor =>
          if !distance.contains(neighbor) then
            distance.put(neighbor, distance(current) + 1)
            queue.append(neighbor)
        }
    distance.toMap

object AlgorithmLaws extends AlgorithmLaws
