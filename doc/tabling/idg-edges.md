# Edges of the incremental dependency graph

Status: phase 1 implemented (compact edge sets).  Phase 2 (reading the
IDG from another thread) is planned.

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

## Planned: reading the IDG from another thread

An external reader increments a global counter before walking and
decrements it after.  A writer that frees a block or an IDG node first
publishes the new state, then checks the counter and, if non-zero, puts
the memory on a lingering list that the last reader frees.
