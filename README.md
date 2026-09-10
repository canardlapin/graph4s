# graph4s

`graph4s` is a small, lawful graph kernel for Scala 3. It separates three jobs
that graph libraries often combine:

```text
GraphExpr[V]  ── compile ──>  Graph[V]  ── index ──>  IndexedGraph[V]
declarative                   persistent               compact CSR

DigraphExpr[V] ─────────────> Digraph[V] ────────────> IndexedDigraph[V]
```

- `GraphExpr` and `DigraphExpr` are compositional descriptions.
- `Graph` and `Digraph` are persistent mathematical values.
- `IndexedGraph` and `IndexedDigraph` are immutable, array-backed snapshots for
  algorithms and numerical code.

The representations are deliberately separate: expressions are easy to
compose, persistent graphs are easy to update, and indexed graphs are fast to
traverse.

> **Project status:** early development on the `0.1` line. The API may change.
> Artifacts are not published yet; build from source for now.

## Mathematical contract

`Graph[V]` is a finite, simple, undirected, loopless graph.

`Digraph[V]` is a finite, simple, directed, loopless graph.

Therefore:

- duplicate edges and arcs have set semantics;
- self-loops are rejected at public construction boundaries;
- every endpoint belongs to the graph;
- undirected adjacency is symmetric;
- directed successor and predecessor indexes agree; and
- every graph can be compiled to a finite integer-indexed snapshot.

Vertex membership uses a coherent Cats `Hash[V]`. Hash-table iteration is not
canonical: reproducible traversal and indexing require `cats.Order[V]` or an
explicitly validated vertex order.

## Quick start

```scala
import cats.Hash
import graph4s.*
import graph4s.syntax.*

given Hash[String] = Hash.fromUniversalHashCode

val roads =
  Graph.of(
    vertices = List("Toronto", "Ottawa", "Montreal", "Halifax"),
    edges = List(
      "Toronto" -- "Ottawa",
      "Ottawa"  -- "Montreal"
    )
  )
// ValidatedNec[GraphBuildError[String], Graph[String]]
```

`Graph.of` treats the supplied vertices as authoritative and accumulates every
self-loop and unknown-endpoint error. `Graph.fromEdges` infers all endpoints:

```scala
val triangle =
  Graph.fromEdges(
    "a" -- "b",
    "b" -- "c",
    "c" -- "a"
  )
```

Directed construction is parallel:

```scala
val pipeline =
  Digraph.fromArcs(
    "read"      --> "parse",
    "parse"     --> "typecheck",
    "typecheck" --> "emit"
  )
```

Both factories return `ValidatedNec`, so an importer can report all malformed
inputs in one pass. Duplicate inputs are harmless.

## Queries and persistent updates

Given a validated `graph: Graph[V]`:

```scala
graph.vertexCount
graph.edgeCount
graph.vertices
graph.edges
graph.containsVertex(vertex)
graph.containsEdge(left, right)
graph.neighborsOf(vertex)
graph.degreeOf(vertex)

graph.addVertex(vertex)
graph.connect(left, right)
graph.connectExisting(left, right)
graph.disconnect(left, right)
graph.removeVertex(vertex)
```

`neighborsOf` and `degreeOf` return `Option`: `Some(empty)` means an isolated
vertex, while `None` means the vertex is absent. Queries do not throw for
missing vertices.

`connect` inserts missing endpoints; `connectExisting` reports them. Removing a
vertex also removes its incident edges. Updates are persistent and idempotent
where the mathematical operation is idempotent.

The graph itself is intentionally not a Scala collection. Explicit operations
avoid ambiguous meanings for `iterator`, `map`, `filter`, or `size`:

```scala
graph.vertices
graph.edges
graph.inducedBy(keepVertex)
graph.spanningBy(keepEdge)
graph.vertexCount
graph.edgeCount
```

## Algebraic expressions

```scala
import graph4s.expr.*

val star =
  GraphExpr
    .vertex("hub")
    .join(GraphExpr.vertices("a", "b", "c", "d"))

val graph: Graph[String] = star.compile
```

`GraphExpr` has four constructors: `Empty`, `Vertex`, `Overlay`, and `Join`.
`DigraphExpr` replaces `Join` with directional `Connect`. Both expression types
have lawful Cats `Functor` instances, and their compilers and mapping
interpreters are stack-safe for deeply nested trees.

`Connect(x, y)` adds every arc from a vertex in `x` to a vertex in `y`; it is
not sequential pipeline composition. Use `DigraphExpr.path` when only adjacent
pipeline arcs are intended.

## Transformations

The undirected core currently provides:

```scala
left.overlay(right)          // equal labels are merged
left.disjointUnion(right)    // Graph[Either[V, W]]
left.join(right)             // Graph[Either[V, W]]
left.cartesianProduct(right) // Graph[(V, W)]
graph.complement
graph.inducedBy(keepVertex)
graph.spanningBy(keepEdge)
graph.lineGraph              // Graph[Edge[V]]
graph.bidirected             // Digraph[V]
```

`Digraph` provides overlay, induced subgraphs, transpose, and conversion to its
underlying undirected graph. Induced-subgraph operations return a structured
result containing the source, result graph, and omitted vertices rather than
discarding that relationship.

## Indexed snapshots

```scala
import cats.Order
import graph4s.indexed.*
import graph4s.indexed.syntax.*

given Order[String] = Order.fromComparable

val indexedResult =
  graph.indexed(VertexOrder.by(Order[String]))
// ValidatedNec[IndexingError[String], IndexedGraph[String]]
```

Indexing is validated because an explicit order may contain duplicates, omit
vertices, include foreign labels, or exceed array addressability. A successful
`IndexedGraph` stores sorted CSR adjacency; `IndexedDigraph` stores CSR
successors and CSC predecessors.

Vertex and edge IDs are path-dependent opaque types. An ID from one snapshot
cannot be passed to another snapshot, even though both erase to integers:

```scala
indexed.vertex("Toronto").map(indexed.neighbors) // valid

// other.neighbors(vertexFromIndexed) does not compile
```

For dense arrays and numerical kernels, `indexed.ordinal(vertex)` is an
explicit escape to a zero-based `Int`. `indexed.vertexAt(ordinal)` safely
re-enters that snapshot's scoped vertex domain. Snapshot arrays remain private,
and neighbor slices expose read-only indexed access.

## Algorithms and evidence

```scala
import cats.Order
import graph4s.algorithms.syntax.*

given Order[String] = Order.fromComparable

graph.bfsFrom("Toronto")
graph.dfsFrom("Toronto")
graph.shortestPath("Toronto", "Montreal")
graph.connectedComponents
graph.requireConnected

digraph.bfsFrom("read")
digraph.weaklyConnectedComponents
digraph.stronglyConnectedComponents
digraph.topologicalSort
digraph.requireDag
```

Algorithms return structured results containing orders, distances, parents,
paths, components, cycle witnesses, spanning trees, topological orders, or DAG
layers as appropriate. Empty graphs have an empty component vector.

Traversal implementations compile to the indexed representation and use
private mutable arrays, queues, and stacks. This is an implementation detail:
results remain immutable and referentially transparent.

`requireConnected` and `requireDag` validate a property once and return
evidence-bearing wrappers. A connected graph retains a spanning tree; a DAG
retains its topological order and layers.

## Topology and data

Annotations are separate from topology:

```scala
import graph4s.data.*
import graph4s.data.syntax.*

val distances = EdgeField.total(graph)(_ => 1.0)
val weighted  = WeightedGraph.from(distances)

val active     = graph.inducedBy(_ != "Halifax")
val restricted = active.restrict(distances)
// Either[TopologyMismatch[String], EdgeField[String, Double]]
```

`VertexMap` and `EdgeMap` are partial maps on one graph. `VertexField` and
`EdgeField` contain exactly one value for every vertex or edge. `ArcMap` and
`ArcField` provide the directional equivalents. Weighted graph wrappers pair a
topology with a total edge or arc field and reject topology mismatches.

Total edge and arc fields can be aligned with an indexed snapshot:

```scala
val index   = graph.indexed(VertexOrder.by(Order[String])).toOption.get
val weights = WeightedGraph.from(distances)
val aligned = weights.indexedBy(index)
// Either[TopologyMismatch[String],
//        IndexedEdgeField[String, Double, index.type]]
```

The aligned field stores values in the snapshot's edge order. Its type retains
that exact snapshot, so a coordinate from another snapshot cannot access it.

## Gale numerical operators

`graph4s-gale` is the optional numerical continuation for JVM and Scala.js:

```scala
import graph4s.gale.*

val indexedWeights = weights.indexedBy(index).toOption.get

val adjacency = indexedWeights.weightedAdjacency
val laplacian = indexedWeights.combinatorialLaplacian
val spectrum  = indexedWeights.vertexSpectrum(rank = index.vertexCount)
```

It provides topology and weighted adjacency, incidence matrices, degree and
strength signals, combinatorial and normalized Laplacians, spectra, spectral
embeddings, and explicit spectral feature similarities. Matrices and solver
results expose Gale types directly; graph4s does not introduce a second linear
algebra abstraction.

Numerical row and column coordinates remain owned by `IndexedGraph` or
`IndexedDigraph`. Derived operators, spectra, embeddings, and feature sets
retain their exact snapshot type. Reindexing means constructing another
validated snapshot with a different `VertexOrder`, then explicitly aligning the
total edge or arc field to it.

Feature similarities compare ordered labels with each snapshot's `Hash`
equality. If two snapshots contain the same labels in a different order,
`alignTo` returns a feature set in the target snapshot's row order. A missing or
incompatible label produces `GraphSimilarityError.BasisMismatch`.

## Cats integration

The library provides only instances with clear graph semantics:

- extensional `Eq` through `PartialOrder`;
- `Show`;
- `PartialOrder` by subgraph inclusion;
- `BoundedSemilattice`, with overlay as the operation; and
- order-independent `Hash` for undirected `Edge`.

There is deliberately no `Functor`, `Foldable`, `Traverse`, or `Monad` for a
compiled graph. Relabeling and quotienting have consequential collision
semantics and will use explicit operations rather than a misleading `map`.

## Modules

| Module | Purpose |
| --- | --- |
| `graph4s-core` | Graph values, validated builders, updates, transformations, and Cats instances |
| `graph4s-expr` | Algebraic graph expressions and stack-safe compilation |
| `graph4s-indexed` | Graph-scoped IDs, CSR, and CSC |
| `graph4s-data` | Partial and total fields, topology checks, and weighted graphs |
| `graph4s-algorithms` | Traversal, paths, components, SCCs, and graph-property evidence |
| `graph4s-gale` | Optional Gale-backed sparse operators, spectra, embeddings, and similarities |
| `graph4s-laws` | ScalaCheck generators and Discipline rule sets |

The core, expression, indexed, data, algorithms, and laws modules cross-build
for the JVM, Scala.js, and Scala Native. `graph4s-gale` targets the JVM and
Scala.js, matching Gale's supported platforms. The pure kernel depends on Cats
and cats-collections, not Cats Effect or Gale.

## Build from source

Prerequisites are a JDK and [sbt](https://www.scala-sbt.org/).

```bash
git clone https://github.com/canardlapin/graph4s.git
cd graph4s
sbt compileAll
sbt testAll
```

Gale is currently consumed from an immutable GitHub source revision. For
coordinated development against a sibling checkout:

```bash
sbt -Dgraph4s.gale.build=../gale galeJVM/test galeJS/test
```

To check formatting:

```bash
sbt scalafmtCheckAll
```

The test suite covers structural invariants, algebra laws, expression
stack-safety, indexed round trips, and conformance with independent reference
algorithms across the supported platforms.

## Scope

The current foundation intentionally defers multigraphs, loops, hypergraphs,
weighted traversal algorithms, bipartite and tree evidence,
quotient/relabeling reports, morphisms, lazy views, streaming codecs, and
interoperability adapters. These structures have different laws or need
explicit semantic choices; they will not be hidden behind configuration
parameters on `Graph[V]`.

See [the design review](docs/design-review.md) for architectural decisions,
implemented scope, and deferred work.

## License

Licensed under the [Apache License 2.0](LICENSE).
