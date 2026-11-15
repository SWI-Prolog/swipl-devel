# Dropping intermediate tables

Status: experimental, implemented behind the Prolog flag
`table_drop_intermediate` (default 0: off).

## Problem

Tabling naive reverse creates a table for each suffix of the list.
Each table is used once, by the table above it, but the variant table
and the answer tables of all suffixes remain: memory is quadratic in
the length of the list (257Mb for 2000naiveReverse).  Tabled DCGs and
other predicates whose variants carry the remaining input behave the
same.

## Policy

When the root component (the outermost tabled goal) completes, we walk
its tree of completed child components from the leaves up.  The
component tree is retained until the root is freed and its depth is the
position in the chain.

A child component is a candidate if it completed with a single table
that is private, not incremental and has no conditional answers (the
answer trie is `TRIE_DISCARDABLE`).  For each candidate we record when
it completes:

  - Its *cost*: the inferences from its creation to its completion.
    Its *own cost* excludes its completed child components.
  - Its *size*: answer trie nodes plus the compiled clause.
  - The `gen_call` count of the trie, to detect calls to the table
    after completion.

Dropping a table means a later call costs its *recompute cost*: its own
cost plus the recompute cost of the dropped tables below it.  We keep a
table if

  - it is smaller than 1Kb (`TABLE_DROP_MIN_SIZE`): dropping it saves
    little and only risks recomputation,
  - it was called again after it completed (fib/2 tables: all kept), or
  - its recompute cost exceeds F times its own cost, where F is the
    value of the flag.

For a uniform chain, F behaves as "keep every Fth table".  It adapts to
chains with varying costs.  Inferences are cheap and deterministic, but
do not count work in C such as trie insertion and compilation.

## Releasing memory

Pruning the variant paths of dropped tables put the nodes on the free
list of the variant table, so the memory was not released (48Mb of
chunks holding 94k live nodes).  Chunks now count their live nodes.  If
the free list holds more nodes than the trie and twice as many as after
the previous sweep, empty chunks are freed (`sweep_node_chunks()`).  The
nodes of a dropped variant path are usually contiguous, so whole chunks
become empty.

Deleting a node in a full chunk took the chunk lock (20% of the time
when dropping).  This is only needed if atom-GC may be registering the
chunk, which requires that an earlier atom-GC found it full.

## Results

Same binary, flag 0 vs 10, best of 5:

| Benchmark | Time | Memory |
|-----------|-----:|-------:|
| 500naiveReverse | 121% | 15% |
| 1000naiveReverse | 118% | 20% |
| 2000naiveReverse | 123% (163 -> 200ms) | 17% (258 -> 44Mb) |
| other benchmarks | 96-106% | 100% |

Other benchmarks keep their tables (small or reused).  The extra time
for naive reverse is pruning about a million nodes one by one.  Part of
this work is otherwise done when the tables are abolished, but clearing
a whole variant table chunk by chunk is cheaper than pruning paths.

## Open questions

  - Peak memory is not reduced: all variants of the chain exist before
    the first table completes.  Dropping a child when its parent
    completes rather than at the root would reduce the peak of the
    answer tables, but not of the variant table.
  - CPU time would count the work in C, but is noisier and costs a clock
    call per table.
  - Evicting tables across queries, e.g., least recently used when
    `table_space` is exhausted (now a resource error), needs the time of
    the last use of a table.
  - The minimum size is a constant.
  - Combines with [jumbo nodes](jumbo-nodes.md): these reduce the size of
    the remaining tables and the cost of pruning.
