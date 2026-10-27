# ErgoAI cyc150: where memory and time go

Status: analysis; proposals 3, 4 and 5 implemented, 2 in part.

The cyc150 programs of ErgoAI-performance are 144 questions against one
Cyc-derived KB, run by ergoAIp (ErgoAI on SWI-Prolog).  The TPLP
benchmarks are synthetic, so cyc150 is the workload to optimise for.
How to run them is in `~/src/prolog/ErgoAI/doc/tooling.md`.

## The workload

All cases behave alike: 142 of the 144 solve in 33–42 s with
283–343M inferences at a 13–17 GB peak (full run of 2026-10-25); the
two `subeventsofsphase` cases exhaust a 40 GB table space.  The
measurements below use `kbq-cpttest-diploidnumberfordogs-2` (load
1.7 s, solve 34.5 s, 330M inferences, peak 14.3 GB), run in-process
(`ergoaip_perf.pl -i --in_process --table-space=40g`).

All tables are incremental: ergoAIp declares every tabled predicate of
the KB and of the argumentation theory (`ergo_gclp`) incremental, and
the stored facts incremental dynamic, so that KB updates re-arbitrate.

### Tables after one solve

| | |
|---|---:|
| entries in the variant table | 12,891,612 |
| of which tables (status `complete`) | 6,384,407 |
| of which dependency nodes of incremental dynamic calls (status `dynamic`) | 6,507,205 |
| tables without answers | 10,467,164 |
| tables with 1 answer | 2,378,292 |
| answers | 2,533,947 |
| IDG | 12.9M nodes, 28.2M edges |
| atoms | 15.3M (1.03 GB), nearly all table blobs |

| Table space | Bytes | Per entry |
|---|---:|---:|
| call trie (variant table) | 3.34 GB | 259 |
| answer tries | 3.14 GB | 243 |
| IDG | 2.60 GB | 201 |
| total pool | 9.07 GB | 704 |
| compiled clauses (outside the pool) | 0.37 GB | 158 per compiled table |

So the memory is almost entirely per-table overhead: about 0.2 answers
per entry, about 700 bytes of tables plus about 70 bytes of atom per
entry.  An empty answer trie is the 176-byte `struct trie`.

Largest predicates (tables, answers, answer-trie bytes):

| Predicate | Entries | Answers | Trie MB | Clause MB |
|---|---:|---:|---:|---:|
| `gm1:'$opposes_f'/2` (dynamic) | 2,623,612 | 0 | 462 | 0 |
| `ergo_gclp:rcand/3` | 1,874,952 | 557,920 | 471 | 67 |
| `gm1:'$cand'/2` (dynamic) | 1,874,952 | 0 | 330 | 0 |
| `ergo_gclp:holds/2` | 1,433,186 | 295,324 | 344 | 40 |
| `ergo_gclp:competes/4` | 874,540 | 874,540 | 595 | 160 |
| `gm1:'$fact'/1` (dynamic) | 769,080 | 0 | 135 | 0 |
| `gm1:'$opposes_open'/2`, `'$opposes4_f'/4` (dynamic) | 437,270 each | 0 | 77 each | 0 |
| `ergo_gclp:` `beater`, `rbeaten`, `erebuts`, `stomped`, `rdefeated`, `rcancelled` | 219k–223k each | 0 or 1 per table | 38–62 each | 0–27 |

The argumentation theory creates about nine tables per literal it
arbitrates, most with no or one answer, and each table's calls to the
incremental dynamic `'$opposes_f'/2`, `'$cand'/2` and `'$fact'/1`
create a dependency node per call variant.

### The cost of incrementality

An experiment build that ignores `incremental` declarations (all other
code equal; the case still passes):

| | incremental | not incremental |
|---|---:|---:|
| solve | 34.5 s | 21.3 s |
| peak RSS | 14.3 GB | 7.9 GB |
| table space | 9.07 GB | 3.45 GB |
| heap in use | 10.9 GB | 4.1 GB |
| `abolish_all_tables` | 28.8 s | 7.1 s |
| inferences | 330M | 323M |

Incrementality costs 38% of the solve time and about two thirds of the
memory, for the same Prolog work.

### What incrementality buys

The programs only use the first completion.  A real workflow asks
several questions and changes the KB.  Loading the core once, then
adding three cases one after the other without abolishing (CPU time):

| Step | Time | Inferences |
|---|---:|---:|
| case A, cold | 34.2 s | 292M |
| case A again | 0.00 s | 43 |
| add case B's facts and rules | 0.12 s | |
| case B, tables kept | 11.5 s | 89M |
| add case C | 0.33 s | |
| case C, tables kept | 14.0 s | 106M |
| `abolish_all_tables` | 29.6 s | |
| case C, cold | 37.6 s | 305M |

A follow-up question after an update costs a third of a cold solve and
needs no abolish, so incremental tabling is the right model.  The goal
is to make it cheaper.

### Time profile of the solve (perf, 35 s)

| Share | What |
|---:|---|
| ~18% | variant lookup: `follow_node`, `trie_lookup_abstract`, `get_answer_table` |
| ~15% | table blobs: `lookupBlob` (9.3%), `PL_(un)register_atom`, `symbol_trie`, `PL_blob_data` |
| ~6% | IDG: `idg_set_add`, `idg_add_child`, `idg_add_dyncall` |
| 4% | `alloc_from_pool` (malloc) |
| rest | VM, `copy_term`, completion (`free_component`) |

### `abolish_all_tables` (29 s)

2.2 µs per entry.  The main thread spends it in `symbol_trie` and
`PL_blob_data` (two passes over the variant table that map each value,
an atom, to its trie), `reset_answer_table`, removing each IDG edge
from both sets (`idg_free_affected`, `idg_set_delete`) and
`free_to_pool`.  Releasing 12.9M blobs triggers 109 atom-GCs, each of
which scans the atom table and all unregistered trie chunks
(`markAtomsInTries`); this ran in the gc thread, 44% of the CPU.  The
case driver's unexplained 8 s per case is this abolish.

## Proposals

Ordered by expected gain on cyc150.  Memory estimates scale the
per-entry numbers above.

1. **Lightweight dependency nodes for incremental dynamic calls.**  Half
   of the entries (6.5M) only record that a table depends on a call to
   an incremental dynamic predicate.  Each has a variant-table path, a
   complete answer trie (176 B) that never holds an answer, an IDG
   node and a blob atom.  These nodes never need a Prolog handle;
   storing just the IDG node at the variant saves the trie and the
   atom (about 1.6 GB and the blob time for those entries).  Coarser
   dependencies for predicates that are rarely changed would save
   nearly all 4 GB, at the cost of invalidating more on update; that
   is a per-predicate option ergoAIp could use for its rule metadata
   (`'$opposes_f'/2`, `'$cand'/2`).

   *Measured:* the coarse variant exists: `dynamic p/1 as
   (incremental, abstract(0))` records a call as the most general
   variant.  Redeclaring `'$opposes_f'/2`, `'$opposes4_f'/4`,
   `'$opposes_open'/2` and `'$cand'/2` this way after loading the core
   (`tools/ergo_bench_abstract.pl`, with proposals 3 and 4):

   | | per variant | `abstract(0)` |
   |---|---:|---:|
   | solve case A | 33.3 s | 35.5 s |
   | table space | 9.08 GB | 5.61 GB |
   | atoms | 12.97M | 7.59M |
   | RSS | 13.76 GB | 9.87 GB |
   | solve case B, tables kept | 11.7 s, 88.7M inferences | 11.7 s, 88.7M inferences |
   | `abolish_all_tables` | 4.4 s | 2.6 s |

   Of these, only `'$cand'/2` has clauses (490 rules of the core; the
   cases add none); the three `\opposes` stores are empty but cause
   3.5M dependency nodes.  ergoAIp (ergoAIstudio2 49eacd8) now declares
   just the `\opposes` stores `abstract(0)`: table space 6.44 GB, RSS
   11.15 GB.  `'$cand'/2` would save another 0.8 GB, but adding a
   defeasible rule would then re-evaluate all `rcand/3` tables.

   Case B does not change these predicates, so the coarser dependency
   costs nothing here; a workflow that changes the rules would
   re-evaluate more.  About 1.2M dependency nodes remain, mostly
   `'$fact'/1`, so lightweight nodes would then save about 0.3 GB
   instead of 1.6 GB.

2. **Tables without blob atoms.**  The variant table stores the atom of
   each answer trie and the code maps it back with `symbol_trie()`.
   Storing the trie pointer and creating the blob only when Prolog
   needs a handle removes up to 15M atoms (1 GB), most of the 15% blob
   time of the solve, the two `symbol_trie` passes and the atom-GCs of
   the abolish.  The lifetime of a trie then needs a reference count
   for handles instead of atom-GC: the main design work.


   *Partly implemented.*  The trie blob is `PL_BLOB_NOCOPY`: its data
   is the trie itself (no allocation, one indirection less).  The
   variant table of a thread stores tagged trie pointers
   (`triePtrValue()`); a trie gets its atom only when Prolog asks for a
   handle, and a trie without one is destroyed when its variant is
   removed.  The dependency nodes of incremental dynamic calls never get
   an atom.  After case A (with the ergoAIp `abstract(0)` change):
   atoms 9.45M -> 6.44M, RSS 11.08 GB -> 10.63 GB, solve A 30.3 s ->
   29.0 s, solve B 11.2 s -> 10.7 s.  Real tables still get an atom
   at creation, because boot/tabling.pl handles them by atom
   (`'$tbl_variant_table'/6`, `create_table/6`, `'$tbl_leader'/5`, the
   current IDG node); avoiding that needs a non-atom handle with its own
   lifetime management.
3. **Bulk teardown in `abolish_all_tables`.**  When all private tables
   go, edges need not be removed from the sets of the other end, and
   the pool could be an arena that is released at once.  Atom-GC
   should run once at the end, not 109 times.  Target: seconds instead
   of 29 s; this matters for every workflow that resets.

   *Implemented* (`clear_variant_table()`): the variant table is
   emptied in one walk that takes over the answer-trie atoms, the
   tables are reset in atom-index (creation) order, private IDG nodes
   free their edge sets without touching the other end, the IDG
   registry is released at once, and the atoms are released after all
   resets.  `abolish_all_tables` after case A and B: 29.1 s -> 4.4 s,
   plus 0.9 s of atom-GC in the gc thread.  Most of the gain is cache
   locality: resetting in variant-table order took 19 s.  The 134 MB
   of registry slots that used to remain after the abolish are freed.

4. **Do not compile incremental tables with at most one answer.**
   `trie_gen/2` on a one-answer trie exits deterministically, so there
   is no open enumeration that a change could disturb, which is why
   incremental tables are compiled.  2.4M tables have one answer;
   their clauses are most of the 0.37 GB.  Low risk.

   *Implemented* (`tbl_single_answer()`): S_TABLED and S_TRIE_GEN unify
   the answer from the trie.  Solving case A leaves 36,400 instead of
   2,359,176 compiled clauses and 12.96M instead of 15.29M atoms (each
   clause has a clause-reference blob); RSS drops from 14.28 GB to
   13.76 GB and the solve from 34.7 s to 33.5 s.  A table that lost
   answers on re-evaluation is still compiled, because
   `reeval_complete_node()` does not prune the deleted answers.

5. **Smaller structs.**  `struct trie` (176 B) carries the O_TRIE_STATS
   counters (24 B) and fields that answer tries rarely use; `idg_node`
   (120 B) carries the monotonic fields (24 B), O_TRIE_STATS counters
   (16 B) and the registry pointer and slot (16 B).  Moving the rare
   fields to side structs saves about 100 B per entry, 1.3 GB.

   *Implemented:* `idg_node` 120 -> 80 B (monotonic fields in
   `idg_mono`, 32-bit counters), `trie_node` 48 -> 40 B (the delay info
   of conditional answers is in a table, flagged by `TN_DELAYINFO`) and
   `struct trie` 176 -> 152 B (the 8 B smaller root node, flags instead
   of the release hook pointer, 32-bit statistics).  Table space after
   case A: 6.44 GB -> 6.06 GB -> 5.66 GB -> 5.51 GB, RSS 10.63 GB ->
   9.80 GB.

6. **A logical update view for answer tries.**  Then incremental
   tables need not be compiled at all (see the discussion of compiled
   versus trie enumeration: compiled is 1.4–1.55 times faster to
   enumerate, costs 1–2 enumerations to compile and is recompiled
   after every changing re-evaluation; the clause adds 36–55% to the
   trie, which an incremental table must keep).

7. **The call trie (3.3 GB, 259 B per entry)** stores large HiLog call
   variants with little sharing.  No cheap fix is known; proposal 1
   halves its entries.

For ergoAIp (`ergo_gclp`): about nine tables per arbitrated literal,
most with no or one answer (`erebuts`, `stomped`, `rdefeated`,
`rcancelled` have about 220k tables and almost no answers).  Merging
the tables that only decide a status into one, or not tabling the cheap
deterministic ones, would cut the entry count; every entry saved is
about 770 bytes and its share of the creation and abolish time.

## Reproducing

- Table statistics after a run: `tools/tblstat.pl`, loaded into the
  toplevel of `ergoaip_perf.pl -i --in_process` (`tblstat(40)`).  It
  walks the variant table with `'$trie_property'/2`,
  `'$tbl_table_status'/2`, `'$idg_thread_space'/2` and
  `'$idg_thread_summary'/3`; for 12.9M entries this takes about 100 s.
- The workflow experiment, `tools/ergo_workflow.pl`, loads the core with
  `ergoaip_compiler:load_ergo/3`, adds a case with `load_spec(kb, +File)`
  and solves `kb:pretest`, as `scripts/ergoaip_cases.pl` does, but
  without `snapshot/1` and `abolish_all_tables/0`.
- The non-incremental experiment made `tbl_set_incremental_predicate()`
  ignore requests to set incremental (not committed).
