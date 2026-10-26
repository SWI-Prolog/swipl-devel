# Edges of the incremental dependency graph

Status: implemented: compact edge sets and reading the IDG from another
thread.

## Measurement

Pharos (`mysqlpump.exe.facts`): 769,121 variants in the variant table,
465,392 IDG nodes and 7.3M edges.  Each edge is stored twice, in the
child's `affected` and the parent's `dependent` set.  Most sets are tiny:

| Set         | empty | 1 edge | <= 4 edges | median | max    |
|-------------|------:|-------:|-----------:|-------:|-------:|
| `affected`  | 316k  | 117k   | 580k       | 1      | 3,898  |
| `dependent` | 742k  | 12k    | 762k       | 0      | 19,149 |

With the generic lock-free hash tables (`TablePP`, `os/pl-table.c`) the
IDG used 654 MB (`idg_size` summed), more than the 221 MB of
`table_space_used` (which does not include the IDG) and the 150 MB of
the answer tries.

## Implemented: idg_set (src/pl-idgset.c)

A set is a single word: empty, one tagged pointer or a block.  Blocks
are appended to and never move their entries; above 8 entries they have
an open addressing index of `uint32_t` positions with at least two slots
per entry.  Deleted entries keep their pointer with the low bit set and
are revived if the pointer is added again.  The sets hold keys only:
the value of an `affected` entry used to be the child itself, and that
of a `dependent` entry the parent.  The `idg_mdep` chains of monotonic
dependencies, the only real values, are in `child->mono_deps`.

Concurrency: the sets of a private node are written by its owner,
except that deleting an edge to a shared node may come from another
thread.  A node is `concurrent` if it is shared or has an edge to a
shared node.  Writes to the sets of a concurrent node lock a stripe
(shared with the concurrent tries) and replaced blocks linger until the
node is destroyed.  Readers never lock; an enumeration reads the set
again for each step, so it survives the set growing or being cleared.

Result (Pharos, 3 runs each):

|                   | TablePP           | idg_set           |
|-------------------|------------------:|------------------:|
| IDG (`idg_size`)  | 654 MB            | 403 MB            |
| heapused          | 1.27 GB           | 0.93 GB           |
| max RSS           | 1.46 GB           | 1.09 GB           |
| CPU               | 40.2, 40.8, 40.3 s | 38.3, 41.1, 46.3 s |

The CPU time is equal within the noise; perf shows 5.5% fewer samples,
the IDG set operations taking 814 samples where `addHTable` and
`deleteHTable` took 2,681.

## Delayed

  - The index costs as much as the entries for large sets (two 4-byte
    slots per 8-byte entry).  Most of the remaining 403 MB are the
    blocks of large sets and the IDG nodes themselves (about 52 MB).
  - Clearing a private set frees its block, so re-evaluating a node with
    many dependencies grows a new block from scratch.  Keeping the block
    did not seem needed given the timings.

## Implemented: reading the IDG from another thread

An external reader calls `idg_reader_enter()` and `idg_reader_exit()`
around following IDG pointers (src/pl-idgset.h).  Freeing an edge-set
block, an IDG node or the struct of an answer trie goes through
`idg_free_or_defer()`: the writer has unpublished the object, issues a
full barrier and frees at once if there are no readers.  Otherwise the
object goes on a list, under a mutex, that the last reader frees.  The
reader increments the counter and issues a full barrier before reading
any pointer, so a writer that sees no readers frees nothing a reader can
reach.

The variant table of another thread cannot be walked safely: deleting a
variant frees trie nodes and blocks at once.  So the IDG nodes are also
kept in a registry: a chunked array of node pointers per thread and one
for the shared tables.  A registry is allocated outside `LD` and kept
alive by its thread and its nodes, as nodes may be destroyed after the
thread exited.  A reader finds the registry of a thread through
`acquire_ldata()` and enters the reader section before releasing it.

`'$idg_thread_summary'(+Thread, -Nodes, -Edges)` is the first reader.
On Pharos it reads 465,392 nodes and 7.3M edges in 0.02 seconds while
the analysis runs.  Running the analysis with a reader every 0.2 s,
every 2 s or none took 47.0, 47.2 and 47.0 s CPU; the instruction
count is the same as before the registry (529.8G vs 529.7G).
tests/tabling/test_idg_sets.pl reads the IDG of a thread that keeps
changing it; under ASan, disabling the deferral makes it report a heap
use-after-free.

## Implemented: the IDG is table space

IDG nodes, the blocks of their edge sets (including lingering ones) and
the slots of the registry are allocated from the table pool of the
answer trie (`alloc_from_pool()`), so `table_space_used` and
`shared_table_space_used` include them and the `table_space` limits
apply.  Running out raises `resource_error(table_space)` from
`idg_new()` or `idg_set_add()` instead of halting; `idg_add_child()`
adds both halves of an edge or neither.  Not included: the
`mono_deps` and `mono_index_deps` hash tables and the `idg_mdep`
chains of monotonic tabling, which are still malloc'ed.

On Pharos `table_space_used` should become about 620 MB instead of 221
MB (221 + 403 MB from above; not measured), below the default limit of
1 GB.
`'$idg_thread_space'(+Thread, -Bytes)` tells the part used by the IDG;
the memory viewer shows it and the call trie (the variant table) as
parts of the tables of a pool.

## Implemented: summarizing the IDG per predicate

`'$idg_predicate_edges'(+Thread, -Nodes, -Edges)` walks the registry of a
thread (or `shared`) as an external reader and aggregates the IDG per
predicate using the predicate of the answer trie (`data.predicate`):
`node(PI, Count, Invalid)` and `edge(From, To, Count)`, where From
affects To.  On Pharos (465,392 nodes, 7.3M edges, 389 predicate pairs)
it takes 0.064 s from another thread; the same aggregation in Prolog
took 9.3 s in the thread itself.  This is meant for an IDG viewer that
starts at the predicate (or module) level.
