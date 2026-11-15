# Generational (segmented) garbage collection for the global stack

Status: implemented on branch `GEN_GC`, enabled by the Prolog flag
`gc_generational` (default `false`).

## What the paper does

Appleby, Carlsson, Haridi and Sahlin, "Garbage Collection for Prolog
Based on WAM", CACM 31(6), 1988, section *Segmented Garbage Collection*
(pp. 738-740):

- A single variable `GC_B` names a choicepoint. Stack, trail and heap
  above `GC_B` form the *new* segment; everything below it is *old*.
- At GC time, `GC_B` becomes the youngest choicepoint. When a
  choicepoint older than `GC_B` becomes `B` (backtracking or cut), `GC_B`
  is reset to `B`, so the old segment shrinks.
- Old-to-new pointers are found on the *new trail*: the `GC_B`
  choicepoint lies between any old cell and any new data, so binding an
  old cell to new data is always trailed.
- Marking stops at pointers into the old heap. Compaction moves only the
  new heap, and early reset is disabled for the old segment.

## Why the paper's boundary does not suit our targets

Our targets are programs that keep large, long-lived data and that run
mostly deterministically. Highly non-deterministic programs of that kind
are typically buggy.

The paper's old segment survives only as long as the `GC_B` choicepoint
does. Deterministic code cuts or exits through choicepoints all the
time, so `GC_B` quickly falls back to the toplevel choicepoint and the
whole heap becomes "new".

## Design

`LD->gen_bar` separates the old generation `[gBase, gen_bar)` from the
young one. After a GC, `gen_bar = gTop`, so all survivors are promoted.
Backtracking lowers it to `gTop` (`__do_undo()`). Unlike `frozen_bar`,
it never prevents backtracking from reclaiming memory.

A minor GC only marks and compacts `[gen_bar, gTop)`. It needs every old
cell that may point into the young generation. These are found in two
places:

- **The trail.** It holds bindings of cells below `mark_bar`, i.e. older
  than the youngest choicepoint.
- **The remembered set** (`LD->gc.remembered`, `gen_remember()`). It
  holds every other store into an old cell:
  - `Trail()`, `varBindConst()`, `GTrail()` and `TrailAssignment()`
    record a cell they do not trail if it is below `gen_bar`. This costs
    one compare on the untrailed path.
  - `setarg()` (the `nb_setarg/3` family) records its target.
  - `gen_assign(p, w)` (pl-inline.h) is a store for C code that fills a
    cell after a call that can run GC.
  - Survivors of a minor GC are not promoted when the argument stack
    points into the global stack, i.e. while the VM builds a term
    (ENSURE_GLOBAL_SPACE in B_FUNCTOR and friends). In that case the
    old-to-young roots of that GC are added to the remembered set, since
    early reset may have deleted their trail entries. So are old cells
    whose trailed assignment saved a young value, because backtracking
    can restore that value into the old cell.

A first version used trailing itself as the barrier, by keeping
`mark_bar >= gen_bar`. That is wrong: it changes which bindings are
undone on backtracking. `library(pure_input)` relies on an untrailed
`put_attr/3` on a fresh cell that survives backtracking through
`nb_linkarg/3`. If a GC between creating and binding the cell promoted
it, the binding became trailed and was undone. The lazy list then lost
its tail, and `doc2tex` looped. The test `untrailed_binding` covers
this. `mark_bar` therefore keeps its meaning; the barrier is only the
remembered set.

## Implementation in pl-gc.c

Unless noted, all functions are in pl-gc.c.

- **Roots.** `gen_roots_to_term_refs()` copies the values of old cells
  that point into the young generation (from the trail and the
  remembered set) into term references in a foreign frame.
  `gen_term_refs_to_roots()` writes the relocated values back after the
  GC. The existing marking and relocation of term references does the
  rest.
- **`isGlobalRef()`** means "points into the collected area"
  (`>= LD->gc.young_base`). This covers frames, term references,
  foreign frames, gvars and the argument stack.
- **`mark_variable()`** stops at old cells.
- **`is_downward_ref()`** ignores old targets.
- **`sweep_trail()`** only relocates young addresses and values.
- **`sweep_global_mark()`** leaves marks into the old generation alone.
- **`compact_global()`** works from `young_base`. During the collect
  phase, the cell below `young_base` temporarily gets a mark bit, as a
  sentinel like the dummy cell below `gBase`.
- **`early_reset_vars()`** never resets old cells: an old `tard` or `gp`
  counts as marked. The `call_residue_vars/2` case (**) only inspects a
  plain previous trail entry.
- **Attvars.** `untag_trail()` only marks young attvars, and
  `clean_attvar_chain()` only cleans the young part of the chain, which
  is ordered newest first.
- **Promotion.** At the end of a GC, `gen_bar = gTop`. With the flag off
  it is `gBase` instead, which also disables the barrier. A major GC sets
  `gen_bar = gBase` before collecting.
- **`mark_bar`** is not relocated by GC. It is clamped to `gTop` after
  GC.
- **`update_stacks()`** relocates `gen_bar` and the remembered set.

## Policy

The policy lives in `gen_minor_gc()` and the end of `garbageCollect()`.
A GC is major if any of these holds:

- the flag `gc_generational` is false;
- it was requested by `garbage_collect/0` (`GC_USER`);
- the remembered set overflowed (1M entries);
- a stack trim was requested;
- the old generation is larger than 2 × the live size after the last
  major GC, plus 1 MB;
- there have been 50 minor GCs since the last major.

If a minor GC leaves the global stack or trail tight, a major GC runs
next, but only when the old generation has grown by more than 25% since
the last major GC. Otherwise growing the stack is the right choice. All
constants are guesses.

## User-visible interface

- **The flag `gc_generational`** (thread-local, default `false`).
- **`statistics(minor_collections, N)`**: minor GCs are included in the
  `garbage_collection` count.
- **`trace_gc`** reports minor GCs as `gc(minor(...))`, with the same
  arguments as `gc(done(...))`.

## Debugging (debug builds)

- `prolog_debug('MSG_GC_GEN_VERIFY')` (or `-d MSG_GC_GEN_VERIFY`):
  before each minor GC, scan the old generation for old-to-young pointers
  not covered by the trail or remembered set. Also check that no GC mark
  bits remain before and after every GC.
- `prolog_debug('MSG_GC_STRESS')`: request a GC after every 4 KB of
  global stack allocation.
- **Locating a missing root.** The verifier prints the GC number and the
  cell offset. Use a gdb watchpoint on that cell from the previous GC
  onwards. Re-arm it after each `grow_stacks()`, because the stacks
  move.

## Testing

- **`tests/GC/test_gc_generational.pl`** runs with the flag on (and the
  verifier, in debug builds). It covers old-variable bindings, `setarg/3`,
  `nb_setarg/3`, `nb_setval/2`, `b_setval/2`, exceptions, attributed
  variables, reading a long list, backtracking, and untrailed bindings
  that survive backtracking.
  - A mutation test confirms it fails when the remembered set is
    disabled.
  - A second mutation test confirms it fails with the earlier
    `mark_bar`-raising barrier.
- **The full suite** passes in the release build. In the debug build it
  also passes with the flag on by default plus `MSG_GC_GEN_VERIFY` and
  `MSG_GC_STRESS`, except for two failures that also occur with the flag
  off:
  - `core_text`: predicate_kind:library;
  - `cpp:ffi`: a `linkValI` assertion.

## Barrier audit

- **Found by the verifier:**
  - `read_list_resume_elem()` and `readValHandle()` (pl-read.c);
  - unpromoted survivors (two cases, see Design).
- **Found by reading the remaining core C files:**
  - `put_frame_goal()` (pl-trace.c): `ensureTrailSpace()` between
    allocating the goal and filling it;
  - `resort_dicts_in_term()` (pl-dict.c): sorts a dict in place after
    `do_load_qlf_term()` built it, possibly across a GC.
- **Not audited.** Packages that write global-stack cells directly,
  rather than through `PL_*()` functions. Code that uses only the
  foreign-language interface is safe: `PL_unify*()` binds through
  `bindConst()`.

## Measurements

The benchmarks are in the session scratchpad `genbench.pl`. Each keeps a
list of 1M `f(X)` terms (5M cells) alive. Numbers are the best of 3,
pinned to one CPU.

| benchmark | total off | total on | GC off | GC on | RSS off | RSS on |
|---|---|---|---|---|---|---|
| frozen: failure-driven loop with `nb_setval/2` | 0.91s | 0.53s | 0.44s | 0.05s | 94MB | 94MB |
| nbsa: same with `nb_setarg/3` | 0.91s | 0.53s | 0.43s | 0.05s | 94MB | 94MB |
| exc: failure-driven `catch/throw` | 0.76s | 0.67s | 0.20s | 0.09s | 158MB | 158MB |
| det: deterministic churn | 4.96s | 4.06s | 1.11s | 0.31s | 164MB | 165MB |
| grow: most data survives | 0.87s | 0.82s | 0.20s | 0.16s | 164MB | 164MB |

- **Cost with the flag off.** On bench/run.pl the branch executes 0.4%
  more instructions than master. Cycle counts are within noise.
- **Where minor GC time goes.** Marking is almost free. Compaction stays
  linear in the size of the young region, because sliding compaction
  scans every cell.

## Open issues

- Tune the policy constants. The size of the young region could be
  decoupled from the `factor*gced_size` trigger.
- The local stack is still scanned completely, so deep non-tail
  recursion gains little.
- Pre-existing, found along the way:
  - `SIG_TUNE_GC` raised during a pending exception makes
    `PL_get_nchars()` fail (seen in `cpp:ffi` under heavy GC);
  - in debug builds, `CHK_SECURE` asserts on a stale `mark_bar`, which
    GC does not relocate; master does this too;
  - `$seek_list` keeps a raw pointer across `PL_handle_signals()`.
