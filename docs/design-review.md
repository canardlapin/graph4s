# Design review and implementation decisions

The three-stage architecture is the right center of gravity:

```text
description -> persistent mathematical value -> indexed computational domain
```

It keeps symbolic construction, incremental application logic, and tight
algorithm loops from forcing incompatible tradeoffs into one representation.

## Corrections made before implementation

### Construction remains validated when endpoints are inferred

`Graph.fromEdges` and `Digraph.fromArcs` can infer endpoints, but they can
still receive self-loops. They therefore return `ValidatedNec`, just like the
authoritative factories. Returning a bare graph would either make construction
partial or silently discard invalid input.

### Indexing has a typed failure channel

`VertexOrder.Explicit` must be a permutation. It can contain duplicates,
foreign labels, or omissions. CSR/CSC storage is also `Int`-addressed even
though persistent graph edge counts are `Long`. `indexed` therefore returns
accumulating typed errors instead of pretending to be total.

`StructuralHash` is not an ordering policy: hash collisions require a
tie-breaker, and process-local hashes are not scientific artifact identity.
Canonical digests remain a future API requiring both an explicit order and a
canonical vertex codec.

### Traversal choice is explicit

BFS/DFS order, parent maps, topological tie-breaking, and component numbering
are not graph invariants. Public algorithms producing these values require
`Order[V]`; adjacency iteration order never leaks into a reproducibility claim.

### Empty structures are represented honestly

An empty graph has zero connected components, so `Components.components` is a
`Vector`, not a `NonEmptyVector`. It also cannot produce connected-graph
evidence carrying a spanning tree, so connectivity validation distinguishes
`EmptyGraph` from `Disconnected`.

### Directed expression chaining is a cross product

Algebraic `Connect(x, y)` adds every arc from every vertex in `x` to every
distinct vertex in `y`. Chaining it does not mean workflow sequencing. The
documentation and tests make the extra-arc behavior explicit.

### Generic labels are not stored in raw arrays

Integer topology uses private primitive arrays. Generic labels use `Vector[V]`
instead of requiring `ClassTag[V]` or introducing an unchecked generic-array
cast. Label access is not the hot adjacency loop, so this is a favorable
safety/performance tradeoff.

### Validated edges are values in both Scala and Cats APIs

`Edge[V]` and `Arc[V]` are final classes rather than case classes, but they
implement ordinary `equals`, `hashCode`, and `toString`. This makes repeated
edge iteration, standard `Set`/`Map`, `distinct`, and ordinary test assertions
behave as users expect. Universal `Edge` equality is endpoint-order
independent; universal `Arc` equality is directional.

Their Cats `Hash` instances use the graph's coherent `Hash[V]`, so custom Cats
vertex equality remains authoritative inside graph4s and Cats collections.
This is an explicit two-boundary policy: ordinary Scala collections use
ordinary endpoint equality, while graph topology uses Cats equality.

## Representation and performance

Persistent adjacency uses Cats Collections CHAMP `HashMap` and `HashSet`,
parameterized by Cats `Hash[V]`. Every vertex has an adjacency entry, including
isolates. Undirected edge iteration uses a private visited set to emit each edge
once without requiring an arbitrary orientation.

Bulk builders do not replay public persistent updates. They use local mutable
hash buckets keyed by `Hash[V].hash`, resolve collisions with `Hash[V].eqv`,
and freeze into CHAMP values once. Expected construction is linear in input
size under a lawful, well-distributed hash.

Indexed snapshots:

- sort labels once from `Order[V]` or validate an explicit permutation;
- fill CSR/CSC in one edge pass and sort each neighbor segment;
- store undirected adjacency in `2E` integer slots;
- store directed outgoing and incoming adjacency in `E` slots each;
- expose no mutable array;
- use path-dependent opaque `Int` IDs to prevent cross-graph mixing;
- expose an explicit graph-scoped-to-dense `ordinal` escape for primitive
  arrays and numerical kernels.

Algorithms use local mutable queues, stacks, bit arrays, and integer sentinels.
Sentinels are confined to private arrays and converted to `Option` or sparse
typed maps before returning; they are not part of a semantic API. Normal
algorithm execution compiles through `IndexedGraph`/`IndexedDigraph` and reads
the resulting CSR/CSC slices. A persistent fallback exists only for values
whose adjacency cannot fit in one indexed array.

## Lawful Cats surface

The persistent graph provides:

- extensional `Eq`;
- `Show` with explicitly noncanonical iteration;
- `PartialOrder` as the subgraph relation;
- `BoundedSemilattice` as overlay with empty.

It deliberately does not provide `Functor`, `Foldable`, `Traverse`, or
`Monad`. `GraphExpr` and `DigraphExpr` provide stack-safe lawful `Functor`
instances because label collision policy is not needed until compilation.

## Current scope boundary

Implemented now:

- persistent simple graphs and digraphs;
- accumulating builders and persistent updates;
- overlay, induced/spanning subgraphs, complement, disjoint union, join,
  Cartesian product, line graph, transpose, underlying graph, and bidirection;
- expression algebras and named path/cycle/clique/star/biclique constructors;
- validated deterministic CSR/CSC snapshots with graph-scoped IDs;
- partial/total vertex and edge data and weighted undirected graphs;
- BFS, DFS, shortest paths, weak/strong components, topological sorting,
  connected and DAG evidence;
- structural, algebraic, cross-representation, and compile-time ID tests.

Deferred behind separate contracts:

- injective relabeling, quotient reports, and contraction maps;
- directed fields and weighted digraphs;
- bipartite, tree, forest, and strongly-connected evidence wrappers;
- spanning forests, articulation points, bridges, and weighted algorithms;
- morphisms and lazy views;
- FS2, Circe, GraphML, JGraphT, and Graph for Scala adapters;
- canonical digests.

These are not represented by placeholders or weakened general-purpose types.
Each can be added as a module or evidence-bearing result without changing the
meaning of `Graph[V]`.
