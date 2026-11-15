# Tools for debugging tabled programs

Status: analysis; the live SDG (1 below) is implemented.

What XSB and ErgoAI offer to inspect and explain a tabled computation,
what SWI-Prolog lacks and where a graphical view would help.  XSB
presents everything as text or Prolog terms; the only graphical tools in
this family are ErgoAI's (the "Why?" window of Ergo Studio) and Silk's
(Silklipse).

## Sources

  - XSB Manual Vol. 1, v5.1 (2026), sections 6.12-6.15 and 9.2-9.3
    (https://xsb.sourceforge.net/manual1/manual1.pdf).  Vol. 2 has no
    debugging tools for tabling.
  - Ergo Reasoner Manual 1.2, chapter 43; ErgoAI sources on GitHub
    (`docs/extensions/justifier.tex`).
  - Liang and Kifer, Terminyzer, PADL 2013 and TPLP 13(4-5) 2013.
  - Andersen et al., Understanding Rulelog Computations in Silk, WLPE
    2013 (arXiv:1308.4125).
  - Roychoudhury, Ramakrishnan and Ramakrishnan, Justifying proofs using
    memo tables, PPDP 2000; Pemmasani et al., Online justification for
    tabled logic programs, FLOPS 2004.
  - Grosof and Swift, Radial restraint, AAAI 2013.
  - Swift, Forest logging: a trace-based analysis of large rule-based
    computations (submitted to the Semantic Web Journal, 2014).

## Representations in XSB and ErgoAI

### Forest log

`log_forest/1,2` (options `file(F)`, `level(full|partial|calls_only)`,
`set_pred(PredSpec, on|off)`) writes one fact per tabling operation,
numbered by a global counter; `load_forest_log/1` loads it:

| Fact                          | Event                                                    |
|-------------------------------|----------------------------------------------------------|
| `tc(S1,S2,Stage,C)`           | call to S1 from the tree of S2; Stage is `new`, `cmp`, `incmp` or `reeval` |
| `nc(S1,S2,Stage,C)`           | the same for a negative call                             |
| `na(A,S,C)`, `nda(A,S,D,C)`   | new (conditional, delay list D) answer of S              |
| `ar(A,S,ST,C)`, `dar(...)`    | answer of an incomplete S returned to a consumer in ST   |
| `dly(L,ST,C)`                 | negative literal L is delayed                            |
| `cmp(S,SCC,C)`                | S completes in SCC (`ec` for early completion)           |
| `ta(subg(S)\|pred(PI)\|all,C)` | tables abolished                                         |
| `err(S,C)`                    | nearest tabled call of an exception                      |

Clause resolution, answers from complete tables, subsumption,
incremental and abstraction events are not logged.  The overhead is
70-80%; logs of 14M facts have been analysed.  ErgoAI writes the same
events as frames (`\logforest`).

The analysis predicates print text: `forest_log_overview` (subgoals,
SCCs, early and never completed subgoals, a histogram of SCC sizes,
delays and conditional answers), `get_scc_size/2`, `analyze_an_scc/2,3`
(subgoals per predicate and "Calls from p/n to q/m : N", optionally
abstracted, e.g. by argument modes with `abstract_modes/2`) and
`three_valued_sccs/1` (the SCCs whose conditional answers were never
simplified: the cause of undefined answers).

### Dependency graphs

Adjacency lists of `subgoal(Vertex, SCCKey, SubgoalKey, CallsTo,
Answers, PosEdges, NegEdges)`:

  - **SDG**: the incomplete subgoals with signed edges (`get_sdg_info/1`,
    `print_sdg_info/0`).  This is what XSB suggests after a tripwire.
  - **IDG**: incremental dependencies (`incr_directly_depends/2`,
    `incr_trans_depends/2`, `statistics(summarize_idg)`; `get_idg_info`
    is documented as not yet implemented).
  - **RDG**: Subgoal/Answer vertices of undefined answers, with a signed
    edge if one occurs in the delay list of the other.  `get_rdg_info` is
    documented as not yet implemented.

`morph_dep_graph/3` collapses a graph by an abstraction and
`dep_graph_scc_info/2` summarizes its SCCs.

### Undefined answers

`get_residual/2`, `get_residual_sccs/3,5` and `explain_u_val/3`, whose
Reason is `negative_loops(cycle)`, `negative_loops(dependent)`,
`unsafe_negation` or `bounded_rationality`.  Restraint shows up as the
atoms `radial_restraint` and `answer_count_restraint` in delay lists.

### Tables, statistics and tripwires

  - `table_dump/2,3`, `table_state/4`, `subgoal_property/2`: calls,
    answers, truth values and status per subgoal.
  - `exception_pre_action` dumps the incomplete tables and their SCCs
    when an exception is thrown (`get_scc_dumpfile/1`).
  - Tripwires (`max_table_subgoal_size`, `max_incomplete_subgoals`,
    `max_scc_subgoals`, `max_table_answer_size`,
    `max_answers_for_subgoal`, `max_memory`) can `suspend` into a break
    with a diagnosis such as "nearly all of these recursive components
    are trivial".
  - ErgoAI adds `showgoals`, `showtables`, `peek` and `setmonitor`, and
    heuristics: fewer than ~3% active SCCs per active goal suggests badly
    structured rules; above 0.3 and growing suggests unbounded recursion
    on growing terms.

### Terminyzer (ErgoAI)

Analyses a forest log taken when a tripwire pauses the computation and
reports "subgoals that form a possibly infinite call loop" or an
"infinite answer-producing pattern" with their rules and source
locations, and suggests abstraction or delay.  It has a dialog in Ergo
Studio.

### Justification

Ergo's `\why` gives a tree `explanation(TV(Goal, Reason, RuleInfo),
Support)` with TV `true`, `false`, `undefined` or `contradiction`; Ergo
Studio and Silk show it as a coloured tree, with negative loops as
cycles.  The justification papers build an evidence graph of (literal,
truth value) pairs from the tables after the fact (PPDP 2000) or by
program transformation (FLOPS 2004).  XSB's own justifier is "not
currently robust enough to be recommended".

## SWI-Prolog compared

| Representation               | SWI-Prolog                                                   |
|------------------------------|--------------------------------------------------------------|
| Forest log and its analysis  | missing; only `debug(tabling(_))` in debug builds           |
| SDG                          | ahead: `'$tbl_sdg'/3` and the SDG viewer (from the menu, thread monitor or a break) |
| IDG                          | ahead: `tidg`, `summarize_idg`, `'$idg_predicate_edges'/3` and the dependency viewer |
| RDG                          | as text: `call_residual_program/2`, the toplevel residual program |
| `explain_u_val`              | missing; the delay lists hold the information               |
| Graph abstraction            | missing as a library; the dependency viewer does it for the IDG |
| Tables and statistics        | present: `tdump`, `table_statistics`, `get_calls/3`, ...    |
| SCC dump on exception        | missing                                                      |
| Tripwires                    | `prolog:tripwire/2` and `suspend`; `max_incomplete_subgoals` and `max_scc_subgoals` added; the break shows the SDG |
| Terminyzer                   | missing                                                      |
| Justification                | missing                                                      |

## Candidate visualizations

In order of value for the effort:

 1. **Live SDG: why does this not complete?**  The incomplete tables and
    SCCs of a running thread, read from outside like the IDG registry
    (a C helper as `'$idg_predicate_edges'/3`; `'$tbl_scc_data'` only
    works in the thread itself).  Shown with xdot as the dependency
    viewer, negative edges dashed and red, SCCs as clusters, nodes sized
    by answers.  Opened from the thread monitor or a tripwire `suspend`.
    The ErgoAI ratios can be plotted over time in the thread view.
 2. **Undefined explorer (RDG plus explain_u_val).**  From the delay
    lists we have (`get_returns_and_dls/3`, `call_residual_program/2`):
    Subgoal/Answer nodes with signed edges, negative cycles as a
    cluster, each undefined answer labelled with its reason (negative
    loop, depends on one, restraint).
 3. **Forest logging.**  Needs a tabling event stream in the kernel
    (new call with its stage, new answer, delay, completion), e.g. as a
    `prolog_listen/2` channel.  On top of it: a timeline of calls per
    predicate, the answer flow per SCC, replaying the dependency graph,
    and a Terminyzer-like analysis of call loops over growing terms that
    highlights the clauses.  The largest effort and opt-in because of
    its cost.
 4. **Justification tree.**  Reconstructed from the tables as in PPDP
    2000 and shown as a collapsible tree coloured by truth value, cycles
    as back edges.  Most visible to users, but semantically hardest with
    negation and answer subsumption.
 5. **Smaller additions.**  SCC dump on exception (showing the SDG of
    1); SCC histograms and the per-SCC "calls from p/n to q/m" matrix in
    the memory viewer; argument-mode abstraction as a level of the
    dependency viewer between predicate and call pattern.

1 and 2 build directly on the dependency viewer (outside reader, xdot)
and answer questions for which XSB only has text.  3 needs a separate
design of the event hook.

## Live SDG: design and status

The worklists, suspensions and components of a thread are owned by
that thread and change without locks, so unlike the IDG registry they
cannot be read from another thread.  The viewer reads them using
`call_in_thread/2`, which runs at a safe point of the thread.

### Implemented

`'$tbl_sdg'(-Components, -Tables, -Edges)` (`src/pl-tabling.c`)
describes the SDG of the calling thread:

  - `scc(SCC, Parent, Leader)` for the active components, from the
    root(s) down through `children`.  Merged and completed components
    are skipped; the worklists of a merged component moved to the
    component it was merged into.
  - `table(Trie, Variant, SCC, Answers, Flags)` for the worklists in
    `created_worklists` of these components.  Flags are `negative`,
    `pending` (in the component's worklist), `executing` and `delays`.
  - `edge(Caller, Callee, Sign, Count)`, counted from the suspensions
    in the worklist of Callee.  A suspension is a recorded
    `dependency(SrcSkel, Cont, Skel, CallerWorklist, Delays)`; its TNOT
    bit gives the Sign.  Only callers that are in Tables are accepted,
    so a stale pointer is never followed.

`break/0` (and so a tripwire `suspend`) and answer completion hide the
current component by clearing `LD->tabling.component`.
`save_tabling_status()` now links the saved state into
`LD->tabling.saved_status`, so '$tbl_sdg'/3 finds the hidden
components as well.  Answer events of monotonic tables also save the
status, but they do not run inside an active component.

Limitations:

  - Edges come only from suspensions.  A call that is still running,
    such as a parent component running the leader of a child, is on the
    Prolog stack; the parent link of the component stands in for it.
  - Decoding a suspension copies its whole continuation.  Should this
    be too slow on large SDGs, `suspension` can store the caller's
    worklist (8 bytes per suspension).

The viewer is `prolog_sdg_viewer` in xpce's
`library(swi/pce_sdg_viewer)`, a pane of the IDE.  It takes a snapshot
in a thread of its own (`call_in_thread/3` with a 5 second timeout)
and shows it with xdot:

  - At the level of variants the nodes are the tables and the clusters
    the components, nested as the components are.  A component with a
    single table and no children is just its table.
  - At the level of predicates the nodes are predicates; clicking one
    shows its tables as a cluster.  There are no component clusters
    here, as the tables of a predicate may be in different components.
    The level `auto` (default) shows variants up to 60 tables.
  - Negative edges are red and dashed, tables grow with log10 of their
    answers, a leader has a double outline, a table with work left
    (`pending`) a thick one, one waited for by tnot/1 a red one and one
    with conditional answers is yellow.

It opens from the Debug/Tabling menu, the popup of the thread monitor
and when a break starts in a thread with incomplete tables
(`prolog:message_action(break(begin, _), _)` in `swi_ide.pl`).  That
covers the tripwire action `suspend` without a new hook: the
`prolog:tripwire/2` hook does not get the action, and break/0 announces
itself after hiding the components, which '$tbl_sdg'/3 still finds.  A
break interrupting a tabled computation opens it too.  The first break
of a thread without a toplevel (level 0) prints no message and so does
not open the viewer.

Observed: for `win(X) :- move(X,Y), tnot(win(Y))`, a pair of tables in
a negative loop also has a positive suspension in one direction.

### Counters and tripwires

`LD->tabling.incomplete_tables` counts the worklists in the
`created_worklists` of the active components (+1 in
`add_newly_created_worklist()`, -n where such a set is dropped: at
completion, discard, `free_component()` and thread cleanup; a merge
moves sets and keeps the count) and `LD->tabling.active_components`
the components with status `SCC_ACTIVE` (`set_component_status()`).
`'$tbl_incomplete'(Thread, Tables, SCCs)` reads them for any thread
without stopping it.  The thread monitor plots the number of
incomplete tables and the SCCs per incomplete table in percent.

The flags `max_incomplete_subgoals` and `max_scc_subgoals` (with their
`_action` flags: error, warning or suspend) are tripwires.  The first
is checked before a new table is created in `unify_fresh()`, with the
predicate as context; the second after a merge in
`unify_table_status()`, with the table of the call as context.  Both
are points where an exception is cleaned up by the leader as usual.
Like the existing count tripwires they trigger once, when the limit is
crossed.

