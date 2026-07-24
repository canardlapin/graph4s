package graph4s.laws

import cats.Hash
import graph4s.{Arc, ArcInput, Digraph, Edge, Graph, Link, VertexSet}
import org.scalacheck.{Arbitrary, Cogen, Gen}

object GraphGenerators:
  given Hash[Int] = Hash.fromUniversalHashCode

  val smallIntGraph: Gen[Graph[Int]] =
    for
      size <- Gen.choose(0, 12)
      vertices = (0 until size).toVector
      possible =
        for
          left <- vertices
          right <- vertices
          if left < right
        yield Link(left, right)
      selected <- Gen.someOf(possible)
    yield Graph.materialize(vertices, selected)

  given Arbitrary[Graph[Int]] =
    Arbitrary(smallIntGraph)

  val smallIntDigraph: Gen[Digraph[Int]] =
    for
      size <- Gen.choose(0, 10)
      vertices = (0 until size).toVector
      possible =
        for
          source <- vertices
          target <- vertices
          if source != target
        yield ArcInput(source, target)
      selected <- Gen.someOf(possible)
    yield Digraph.materialize(vertices, selected)

  given Arbitrary[Digraph[Int]] =
    Arbitrary(smallIntDigraph)

  given Arbitrary[Edge[Int]] =
    Arbitrary(
      for
        left <- Arbitrary.arbitrary[Int]
        right <- Arbitrary.arbitrary[Int].suchThat(_ != left)
      yield Edge.unsafe(left, right)
    )

  given Cogen[Edge[Int]] =
    Cogen[(Int, Int)].contramap { edge =>
      if edge.first < edge.second then (edge.first, edge.second)
      else (edge.second, edge.first)
    }

  given Arbitrary[Arc[Int]] =
    Arbitrary(
      for
        source <- Arbitrary.arbitrary[Int]
        target <- Arbitrary.arbitrary[Int].suchThat(_ != source)
      yield Arc.unsafe(source, target)
    )

  given Cogen[Arc[Int]] =
    Cogen[(Int, Int)].contramap(arc => (arc.source, arc.target))

  given Cogen[Graph[Int]] =
    Cogen[(Vector[Int], Vector[(Int, Int)])].contramap { graph =>
      val vertices = graph.vertices.toVector.sorted
      val edges = graph.edges.iterator
        .map { edge =>
          if edge.first < edge.second then (edge.first, edge.second)
          else (edge.second, edge.first)
        }
        .toVector
        .sorted
      (vertices, edges)
    }

  given Cogen[Digraph[Int]] =
    Cogen[(Vector[Int], Vector[(Int, Int)])].contramap { graph =>
      val vertices = graph.vertices.toVector.sorted
      val arcs =
        graph.arcs.iterator.map(arc => (arc.source, arc.target)).toVector.sorted
      (vertices, arcs)
    }

  given Arbitrary[VertexSet[Int]] =
    Arbitrary(
      Gen.listOf(Arbitrary.arbitrary[Int]).map(VertexSet.from(_))
    )

  given Cogen[VertexSet[Int]] =
    Cogen[Vector[Int]].contramap(_.toVector.sorted)
