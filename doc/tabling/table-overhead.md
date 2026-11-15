# Overhead of creating a table

Status: in progress.

## Measurement

Creating a new trivial table (`:- table p/1. p(_).`, calling `p(I)` for
300,000 values of I) costs 1.2us in SWI-Prolog and 0.1us in XSB.  This
matters for programs with many small tables, e.g., bintree18 creates
262k tables.

Per table: 35 inferences, about 19,000 instructions, of which 8,800 in
the VM.  The Prolog code path is `start_tabling/3`, `catch/3`,
`create_table/5`, `setup_call_catcher_cleanup/4`, `run_leader/5`,
`activate/3`, `delim/4`, `reset/3`, `completion/3`, `done_leader/4` and
`leader_answers/2`.

Profile (ticks, 300k tables): exception handling around the leader
(`catch/3`, `setup_call_catcher_cleanup/4`, `sig_atomic/1`, the cleanup)
about 40%, `'$tbl_variant_table'/6` 18%, returning the answers 10%,
`delim/4` and adding the answer 10%.

Experiments (us per table): removing the `catch/3` for deadlocks saves
0.05, replacing `setup_call_catcher_cleanup/4` by a plain sequence saves
0.29 (1.20 -> 0.86).

## Cheaper setup_call_catcher_cleanup/4

*Rejected: replacing setup_call_catcher_cleanup/4 by catch/3 plus
explicit restores.*  If the SCC is merged, `run_leader/5` suspends using
shift/1 and the cleanup runs when the frames are discarded later.  A
catch/3 based version would not restore the current IDG node in that
case.  Also, the setup must be atomic with respect to signals, e.g.,
call_with_time_limit/2.

Instead:

  - `callProlog()` (used by sig_atomic/1 for the setup) and
    `call_term()` (used to run the cleanup) call internal deterministic
    foreign predicates (module system, name starting with `$`, not
    transparent) directly rather than opening a query
    (`call_internal_det_foreign()`).  The predicate has no frame of its
    own, which affects the context of errors; hence the restriction to
    internal predicates.  This applies to all uses of
    setup_call_cleanup/3 and friends.
  - The cleanup of the leader is `'$tbl_finished_leader'/4` in C rather
    than finished_leader/4 in Prolog.

1.20us -> 1.08us per table.

## Remaining

  - Qualifying and stripping meta arguments: about 870 instructions per
    table, plus module lookups (`htable_get()`, 520 instructions).
    The Worker passed to start_tabling/3 calls a closure blob, so its
    module is irrelevant.
  - `PL_get_blob()`: 5 calls per table to get tries and worklists from
    their blob handles.
  - `PL_unify_term()` in `'$tbl_variant_table'/6`.
