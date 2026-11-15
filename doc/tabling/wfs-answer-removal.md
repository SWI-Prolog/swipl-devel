# Removing conditional answers during WFS simplification

Status: one bug fixed (42b60f107); one latent hazard, not fixed.

## Fixed: simplification looping on a removed delay set

`simplify_component()` loops while the SCC has answers with delays
(`undefined > 0`) and the last pass made progress.  `answer_delays_on()`
decides whether an answer still has a delay on a given table.  It used
to scan all of `di->delays`, but removing a delay set in
`propagate_to_answer()` only deactivates the set in `di->delay_sets`.
Its elements stay in `di->delays`.  An answer could therefore remain
"delayed on" a table that has no answers left, and the loop never
ended.  `answer_delays_on()` now scans only the elements of live sets.

The ErgoAI tests `defeasible/atck2/temp_paradox2.flr` and two related
tests looped this way.  `tests/tabling/test_wfs_stale_delay.pl` is a
7-rule program reduced from random programs; it hangs without the fix.

## Latent: removed answers are pruned while still in answer clusters

`remove_conditional_answer()` deletes the answer with
`trie_delete(at, answer, true)`, i.e., it *prunes* the node, although
the comment next to it says "cannot prune as may be in worklist delay
lists".  The answer may also still be in an answer cluster of its
worklist (`wl->head`, `CLUSTER_ANSWERS`).

`'$tbl_wkl_work'/6` skips removed answers by testing `an->value == 0`.
That works for a node deleted without pruning.  A pruned node, however,
is freed: to the trie's free list if it was allocated in a chunk, and to
`alloc_pool` otherwise.  `free_trie_node()` does not clear `value`, so
the test cannot recognise a freed node, and the memory may be reused by
a new answer, possibly in another trie.

Moded tables avoid this: `update_subsuming_answer()` deletes without
pruning and sets `wl->needs_answer_gc` if the worklist is executing;
`prune_answer_cluster()` later prunes the nodes with `value == 0`.

### Measurements (October 2026)

Instrumented `build.debug`, logging (1) a removal of an answer that is
still in a cluster of its worklist and (2) a later pairing of that
address, checking whether the node is live and belongs to the
worklist's own answer trie.

| Corpus | Programs | With a removal while in a cluster |
|---|---:|---:|
| random WFS programs (6 predicates, `tnot/1`) | 4,000 | 38 |
| ErgoAI defeasible and general_tests golden files | 428 | 38 |

In the 5 files where a removed address was paired again (e.g. the
`atco_specific` tests), all 162 pairings were legitimate: a new, live
answer of the same table had reused the address.  No stale or foreign
answer reached a continuation.

The likely reason is `clean_worklist()`: pass 0 of
`simplify_component()` cleans the worklists of the SCC, and completion
cleans them again, freeing the clusters before they are paired.  A
problem needs a removal that reaches a worklist of a *different*, still
incomplete SCC whose clusters survive and are paired later.  We have not
been able to construct one.

### Possible fix

Follow the moded-table pattern in `remove_conditional_answer()`:

- if `wl->head == NULL`, prune as now;
- otherwise `trie_delete(at, answer, false)` and set
  `wl->needs_answer_gc`, and let `clean_worklist()` (or
  `prune_answers_worklist()`) prune nodes with `value == 0` before it
  frees the clusters.

Caveat: if a node can appear in more than one cluster, pruning while
walking the clusters must not free it twice.  Because this changes
answer-trie memory handling for every WFS program, it should wait for a
failing test case.
