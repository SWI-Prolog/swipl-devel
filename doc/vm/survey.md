# VM performance survey

Status: **in progress** on branch `vm-perf`.  Done: the candidate-index
ordering bug found with Alpino (d6f978478), 1b (4508feeda) and the
frameless simple unification of 4 (cfd9974e5).  Measured and shelved:
1a, inlining the `B_VARn` fast path and avoiding `memset()` in
`H_RFUNCTOR`/`B_RFUNCTOR`.  See [Results](#results).

A survey of low-hanging fruit in the virtual machine: clause selection
for small static predicates, the cost of a call and exit, last-call
optimisation, the instructions the compiler chooses, and argument
passing.  The source-level debugger must keep working: it needs frames,
their parent chain, the PC, the clause and the variables of each frame.

Measured 2026-11-04 on 25dc63c7a, `build` (RelWithDebInfo, GCC 16,
`VMI_FUNCTIONS=ON`, i.e., tail-calling VMI functions), Zen 5, using
`perf stat`, `perf record -e cpu-clock:u` and callgrind.  Other jobs ran
on the machine during part of the measurements; numbers are good to
about ±5 ns per call.  _Measured_ marks numbers from runs; _guess_
marks estimates.  The scripts and raw results are in `tmp/vm-perf/`
of the source tree (not under version control).

## Profiles

`bench/` (`run.pl --speedup=1`, no `-O`), by C function:

| function                     | share | note                                  |
|------------------------------|------:|---------------------------------------|
| `first_clause_guarded()`     |  9.9% | the linear scan, `pl-index.c:304`     |
| `evalExpression()`           |  6.8% | arithmetic is not compiled without -O |
| `clear_variant_table()`      |  6.7% | moded_path only (abolishes tables)    |
| `S_STATIC`                   |  4.1% |                                       |
| `bvar_cont`                  |  3.6% | argument passing                      |
| `depart_or_retry_continue`   |  2.9% |                                       |
| `do_unify()`                 |  2.6% | mostly from `H_VAR`                   |
| `deep_backtrack`             |  2.5% |                                       |

The rest is a flat tail of VMI functions, each below 2.1%.

s(CASP), `swipl -Dsscasp examples/dyncall/queens.pl`, `?- ? nqueens(6,Q).`
(30.7M inferences, 0.98 s):

  - C level: `copy_term()` 4.0%, `mark_for_copy()` 3.4%, GC about 5%,
    `bvar_cont` 3.8%, `S_TRUSTME` 3.3%, `S_LIST` 3.0%,
    `lookupHTable()` 3.0%, and metacall plus module handling
    (`i_metacall_common`, `S_LMQUAL`, `m_qualify_argument()`,
    `stripModuleName()`) about 6%.
  - Prolog level (profile/2, self time): copy_term/4 12.0%,
    maplist/2 10.8% (plus 18% in children), `$garbage_collect`/1
    5.3%, unifiable/3 4.5% (1.47M calls, 75% fail), functor/3 1.6%
    (1.75M calls, 62% fail).

Alpino (`~/3rdparty/Alpino`, `swipl --nopce -O alpino_bench.pl`,
`bench(suite(g_suite), [limit(400)])`: 37.4 s, 377M inferences, 10M
LIPS), by C function before the fixes below.  The profile is flat;
grouped:

  - Clause selection, about 16%: `first_clause_guarded()` 5.1%,
    `shallow_backtrack` 3.8%, `nextClause()` 2.6%, `S_STATIC`,
    `S_TRUSTME`, `deep_backtrack`.
  - Compiling and freeing asserted clauses, about 12-15%:
    `compileArgument()` 4.6%, `analyseVariables2()` 2.9%,
    `forAtomsInCodes()` 2.5% (from `freeClause()`), `skipArgs()`,
    `Output_0()`, `compileClause()`.  Alpino asserts about 577K clauses
    per 400 sentences (the lexical analysis, `assert_lexical_analysis/3`)
    and retracts them per sentence.
  - Head unification, about 10% over the `H_*` instructions.  The big
    grammar tables (`alpino_lc_in:lc_vc/46` and friends, 360-460 clauses
    of arity 22-52, about 98% of the calls fail) only discriminate on
    argument 1 (57 keys over 373 clauses; all other bound arguments are
    variables in nearly all clauses), so no index can help: the cost is
    unifying the heads.
  - Prolog level (exact profiling): assertz/1 10.9%, lexical analysis
    6.7%, `lc_vc/46` 6.0%, `z_f2/3` 5.3% (see the indexing bug in
    [Results](#results)), about 10M calls each of `when/2` and
    `hdrug_flag/2`.

## Results

Measured on `bench/` interleaved (each variant rebuilt, one round each
in turn, best of 3, pinned to one core) and with `perf stat -r 5`
cycles, alternating the variants.  Single runs are not good enough:
another job on the machine and code placement make the same binary vary
by up to 15% between runs, e.g., an apparent 12.5% gain on nreverse
disappeared with repetitions.

  - **Candidate indexes were not sorted** (FIXED, d6f978478).
    `set_candidate_indexes()` keeps the order of `candidate_indexes()`:
    the good indexes, then deep indexes on poor arguments, then
    combinations.  `existing_hash()` uses the first index whose
    arguments are bound, so for Alpino's `z_f2/3` (130K facts, called
    with arguments 1 and 2 bound) the deep list index on argument 2
    (speedup 8.3) was used instead of the hash on argument 1 (speedup
    7577), which was never realised.  Calls took 8-30 us instead of
    0.1-0.5 us; Alpino's g_suite runs 6% faster.  Test:
    `jit_static:word_frame` in `tests/db/test_jit.pl`.
  - **1b** (ENHANCED, 4508feeda).  A few catch-all clauses no longer
    make an argument unindexable if their bucket entries take no more
    space than the clauses.  7th entry of an 8 clause table with a
    catch-all: 59 ns to 27 ns.  `bench/` unchanged (99.1%, noise).
    Test: `jit_static:catch_all`.
  - **Frameless simple unification** (ENHANCED, cfd9974e5).  The
    compound case of `do_unify()` is a separate function, so the
    simple case (`H_VAR` and others) needs no stack frame.  `perf stat
    -r 5`: boyer and chat_parser 2% and zebra 4.5% fewer cycles;
    `bench/` 98.8%.
  - **1a, shelved.**  Falling through to the hash section when the
    small-predicate scan is nondeterministic makes `m_bound_x` (arg 1
    variable or `x`, arg 2 discriminating) deterministic (24-55 ns to
    17-19 ns).  Unconditionally it made the symbolic differentiation
    programs (derive, divide10, log10, times10, ops8, whose `d/3` ends
    with a catch-all) 9-16% slower.  With a filter that only falls
    through if an index on another bound argument exists, decided by a
    flag computed in `set_candidate_indexes()`, it still costs 3% more
    instructions on times10 and ops8 (`perf stat`), while master and
    the sorting fix execute the same number.  A catch-all last clause
    is common, the mixed first argument is not.  The patch is in
    `tmp/vm-perf/indexing/pl-index.c.1ag`.
  - **Inlining the `B_VARn` fast path, shelved.**  Removes the store and
    reload of the variable offset through the register file and makes
    the instructions frameless.  0.1-0.6% fewer instructions, cycles
    within ±1-3% without a direction (`perf stat -r 5`).
  - **No `memset()` in `H_RFUNCTOR`/`B_RFUNCTOR`, shelved.**  A switch
    for arity 1-4 and a loop GCC cannot turn into `memset()` remove the
    call, but both keep a stack frame for other reasons
    (`bind_attvar_const()`, spills).  log10 2.4% and poly_10 1% faster,
    chat_parser 2-4% slower.

These agree with `doc/tabling/tailcall-vm.md`: removing a few
instructions from a VMI does not show unless it breaks a dependency
chain or removes real work.

The branch as a whole against master (`src/pl-index.c` and
`src/pl-prims.c` swapped, alternating):

  - Alpino, g_suite `limit(200)`: 12.47 s to 11.68 s CPU (-6.3%),
    verification OK.
  - s(CASP) `nqueens(6,Q)`: 0.95 s either way.
  - `bench/` best of 3: geometric mean 101.8%, from 96.5% (browse) to
    107% (derive).  With `perf stat -r 5`, derive, det and query are
    within 1%, but pingpong uses about 5% more cycles with fewer
    instructions.  Its extra time is in functions neither change
    touches (`I_FEXITNDET`, `I_DEPART`, `put_fastheap()`,
    `set_trie_value_word()`): code placement.

Open leads found on the way:

  - `forAtomsInCodes()` is 21% of a synthetic assert/retract benchmark
    (arity 40 heads with nested terms; `tmp/vm-perf/assert/`): 0.7 us
    per clause to find the atoms to unregister when freeing it.  Each
    step is a chain of dependent loads (code, `dewam_table`,
    `codeTable[op].arguments`).  Not walking the code, e.g., keeping
    the atoms of a clause aside or having AGC scan clauses, is a design
    decision.
  - `analyseVariables2()` is another 19% of that benchmark: since it
    is iterative, each compound pushes an 80 byte `av_frame`, also
    for compounds with only atomic arguments.
  - Caching the procedure of `call/N` (2) needs invalidation whenever a
    procedure is created, imported or freed or module inheritance
    changes, because `resolveProcedure()` searches the default
    modules.  Not done.

## Candidates, ranked

### 1. Clause selection for small static predicates

How it works now (`S_STATIC` → `firstClause()` →
`first_clause_guarded()`): with at most `MIN_CLAUSES_FOR_INDEX` (10)
clauses and the primary argument bound, the clause list is scanned on
`cref->d.key`; key 0 (a variable) matches anything.  For clean static
code a skipped clause costs a load of the clause reference and a
compare.  After a match, `next_clause_primary_index()` looks ahead up
to `MAX_LOOKAHEAD` clauses for a second match; without one, no
choicepoint is made.  `set_candidate_indexes()` runs once, on the
first call that gets past the fast path, after which `fixed_indexes`
is set.  The primary argument of a predicate with fewer than 10
clauses is the first argument whose keys are not all the same, not
the best one.

Cost model (measured): a hash lookup costs 16–18 ns regardless of
size; the scan 15.5 ns at 5 clauses, about 18 at 10, 23–26 at 20.  The
cut-over at 10 is right.

**1a. Do not trust a nondeterministic fast-path result**
(`pl-index.c:680-694`).  When the scan leaves `chp->cref` set, go on to
the hash section (which also runs `set_candidate_indexes()`), use its
result if it is deterministic and fall back to the scan result
otherwise.  This keeps the #1386 fix (b9a860cf1).  Effort S, risk low.

| call (measured, ns)                                | now          | via hash    |
|----------------------------------------------------|--------------|-------------|
| `m_5(x,k3,_)`: arg 1 var or `x`, arg 2 keys         | 36.2, nondet | 16.7, det   |
| `m_10(x,k5,_)`                                      | 51.3, nondet | 16.0, det   |
| `v10(k1,1)`: one catch-all clause in arg 1          | 17–29, nondet| 13.9, det   |

A tail-recursive loop calling `m_10(x,k5,_)` keeps every choicepoint
and overflowed the stack after 4.3M iterations.  A predicate that is
always called with its primary argument bound never gets another index
(`m_10` shows `indexed=[]`).

**1b. Relax `MAX_VAR_FRAC` for small predicates**
(`assess_remove_duplicates()`, `pl-index.c:2676`).  One variable clause
among fewer than 10 keys exceeds 0.1, so a "default last clause" table
is never indexed on that argument.  8 clauses with a catch-all in arg
2, called as `q(_,g)`: 66.9 ns; with `ci_max_var_fraction=0.5`
22.0 ns (measured).  For example, allow it below about 32 clauses or
for at most 1–2 variable clauses; the speedup estimate already charges
for variable clauses.  Effort S, risk low.

**1c. Choose the primary argument by quality** in
`preferred_primary_index()`: the highest assessed speedup and the
fewest variable clauses rather than the first argument that varies.
Also make `update_primary_index()` (`< MIN_CLAUSES_FOR_INDEX`,
`pl-index.c:3756`) and the fast path (`<=`, line 682) agree.  Overlaps
with 1a; gain 5–10 ns on cases like `ms_10` (guess).  Effort S.

**1d. Deep primary key** when the primary argument has the same
functor in all clauses (what `update_primary_index()` calls "maybe
deep"): compute `cref->d.key` from the nested position and dereference
the same path at call time.  `d_N(f(K),_)` takes 28–32 ns against 16 ns
for the flat equivalent; wrapping the nodes of an AST interpreter as
`e(...)` costs 20% (93.2 against 77.7 µs) (measured).  Effort M, risk
medium (`nextClause()` must agree).

Not worth it now: a second key in the clause reference (a hash lookup
costs about as much as a 10-clause scan once 1a is in), and a compiled
decision tree per predicate: clause selection is about 5 ns of a
deterministic call (`first_clause_guarded()` + `firstClause()` are
33.5% of an `a1_5` loop); effort L, risk high.

Already fine (measured): deterministic first-argument calls
(12–18 ns), discrimination on argument 2 or 3 when argument 1 is the
same constant or always a variable (the primary moves there), and
dispatch on the type of argument 1.

### 2. call/N

`i_metacall_common` (`pl-vmi.c:5500`) does, on every call(G, A...),
`stripModule()`, `lookupFunctorDef(name, arity+N)` in the global functor
table and `resolveProcedure()` in the module's procedure table, then
shifts the closure arguments.

Measured, 5M iterations of `l(N) :- p(N), N1 is N-1, l(N1)`:

| loop                          | time    |
|-------------------------------|--------:|
| direct `p(N)`                 | 0.061 s |
| `call(p, N)`                  | 0.121 s |
| `call(q(a), N)`               | 0.178 s |
| `call(user:p, N)`             | 0.143 s |
| hand-written list recursion   | 0.055 s |
| `maplist(p, L)`               | 0.141 s |
| `maplist(p, L)` with `-O`     | 0.050 s |

So call/N roughly doubles the cost of a trivial call, and maplist/2
without apply_macros is 2.6× slower than its expansion.  s(CASP)
spends 10.8% self time in maplist/2 although `sscasp/solve.pl` loads
apply_macros (closures that cannot be expanded, or other modules).

Candidate: cache the lookups, e.g., a "same name, arity+k" link in
`FunctorDef` and a small per-thread (module, functor) → procedure cache
invalidated by a generation that is bumped when procedures are created,
imported or abolished.  Effort S–M, risk low–medium (invalidation).

### 3. Compile arithmetic by default

With `-O` the `bench/` average drops from 0.654 to 0.582 s (11%);
crypt, perfect, sendmore, query and queens_8 run 1.7–2× faster and ops8
1.5× (measured).  Without `-O`, `evalExpression()` is 67–73% of the
samples of queens_8 and crypt and comparisons compile to
`I_CALL((=<)/2)`; only `NewVar is Var±SmallInt` (`A_ADD_FC`) is
compiled.  Practically all of the `-O` speedup is arithmetic.

What blocks making it the default:

  - Under `-O`, a clause whose arithmetic cannot be compiled, such as
    `X is foo+1` or `X is Y+1` with Y fresh, is rejected at load time
    ("0 clauses") instead of raising the error at runtime.  The default
    must fall back to `I_CALL`.
  - The tracer loses the call port of `1>0`: `A_ADD_FC` and the
    `B_UNIFY`/`B_EQ` instructions check `PLFLAG_VMI_BUILTIN`, the `A_*`
    instructions do not.  Decompilation and `$clause_term_position`
    already handle `A_*`.  Under debug mode, `A_ENTER` could jump to an
    `I_CALL` of the decompiled goal and continue after the block; for
    that it needs the offset of the end of the block.
  - `-O` also drops debug/3 and assertions, simplifies control
    structures in `expand.pl` and enables apply_macros, so arithmetic
    needs a flag of its own.

Effort M, risk medium (error messages must keep their context term).

Follow-ups:

  - **Fused comparisons** `A_<op>_VV(v1,v2)` and `A_<op>_VC(v,int)`.
    Comparing two variables is 56% of all executed arithmetic blocks
    (3.2M of 5.9M; dynamic count over `bench/` with `-O`), each now
    `A_ENTER A_VAR A_VAR A_<cmp>`.  In queens_8 with `-O`,
    `a_var_n` is 10.6%, `A_ENTER` 4.8%, `a_cmp_out` 4.1%.  Expected
    5–15% on queens, tak, qsort and sendmore (guess).  Debugger: a
    single `VIF_BREAK` instruction that decompiles to `X<Y`, with the
    same `PLFLAG_VMI_BUILTIN` fallback as `B_EQ_VV`.  Effort S–M.
  - **Inline `A_SUB`** and `is` fusions for var/var add and mul.  `-`
    compiles to `A_FUNC2((-)/2)` and runs the generic `ar_func_n()`:
    6.3% in crypt (measured).  Effort S.
  - **Store-forwarding stall in `evalExpression()`.**  66% of its
    samples fall on copying `n_tmp` into the evaluation stack
    (`pl-arith.c:1075`), right after narrow stores to it; crypt has 670M
    store-to-load interference events without `-O` and 210M with it
    (measured; attribution uncertain because of sampling skid).  Build
    the number in place.  This matters for every is/2 that is not
    compiled.  Effort S.

### 4. Call and exit

Instructions and cycles per call (measured; difference between two
loop sizes, pinned):

| case                                              | instr | cycles |
|---------------------------------------------------|------:|-------:|
| det call and exit of the fact `q0.`               |   113 |     16 |
| fact `q3(_,_,_)` with atom arguments              |   158 |     25 |
| `r0 :- q0.` (LCO: `L_NOLCO` + `I_LCALL`), extra   |  +102 |     41 |
| `s0 :- q0, true.` (no LCO)                        |   225 |     44 |
| `v3(A,B,C) :- q3(A,B,C).` (arguments in place)    |   260 |     53 |
| `u3`, permuted arguments (`I_DEPART` + copy)      |   328 |     66 |
| `x3`, a compound argument                         |   442 |     93 |

Per function for a fact call: `I_CALL` 14.5, `normal_call` 14.5,
`depart_or_retry_continue` about 19, `S_TRUSTME` 22, `I_EXITFACT` 9 +
`exit_checking_wakeup` 8 + `I_EXIT` 5, `exit_continue` 19.6.  In a
single-clause loop 25% of the cycles wait on a load: in `exit_continue`
(40% of those, `FR->programPointer`), `I_CALL` (12.5%, `FR->level`),
`S_TRUSTME` (11%, `cref->value.clause->variables`) and `normal_call`.

The tail-call VM work found that removing all mode tests saved 1.2% of
the instructions and no cycles (337bc72b3, `doc/tabling/tailcall-vm.md`).
Candidates that only remove instructions are therefore unlikely to show
in timings; those that break a dependent load chain are more promising.

Breaking stalls:

  - **Merge `normal_call` into `I_CALL`** (`pl-vmi.c:1877-1960`) and keep
    `NFR` and `DEF` in locals.  `normal_call` reloads `REGISTERS.nfr`
    from memory five times and `I_CALL` stores and reloads `NFR` and
    `DEF` because of aliasing; the frame stores take their address from
    the reloaded `NFR`.  The same applies to the `I_DEPART` fallback.
    8–10 instructions per call (measured); 3–8% of cycles on call-heavy
    code (guess).  Effort S; frames are unchanged.
  - **`S_TRUSTME`: break the chain `cref->value.clause->variables`**
    (`pl-supervisor.c:260`, `pl-vmi.c:3618`), e.g., by putting the clause
    or its variable count and code in the supervisor, and move the
    stack-growth and `CHP_DEBUG` paths of `TRUST_CLAUSE` out of line.
    `S_TRUSTME` is 15% of the cycles of a single-clause loop.  Effort M:
    GC (`pl-gc.c:1363`, `pl-gc.c:2010`), the decompiler and `S_LIST`
    walk the supervisor code.

Removing instructions (each about 1%, guess):

  - **`H_VAR`** (`pl-vmi.c:687`): 89 instructions and 13 cycles even if
    both words are identical.  It calls `do_unify()`, which reserves
    0x178 bytes of stack for the compound agenda and calls
    `unify_simple_ptrs()`.  Make the simple case inline (or split the
    compound loop into a noinline function).  `do_unify()` +
    `unify_simple_ptrs()` are 4.3% of `bench/`, `H_VAR` 1.5%.  Effort
    S–M.
  - **`B_VARn`** (`pl-vmi.c:1107-1140`): stores the slot offset in the
    register file and `bvar_cont` reloads it (store-to-load
    dependency).  An argument costs 21.5 instructions, about 5 cycles,
    linear in the arity.  Inline the fast path: about 10 instructions.
    Effort S.
  - **`I_EXITFACT`** (`pl-vmi.c:2312-2370`): fold `LD->yield.frequency`
    (WASM only) into an `ALERT_YIELD` bit, so that a single
    `LD->alerted` test leads to `exit_continue`, and move the
    `tracePort()` code to a VMH.  22 instructions and 3 jumps become
    about 5.  Effort S.
  - **`I_LCALL`** (`pl-vmi.c:2582`): calls `contextModule(FR)` always,
    which walks the parent frames, but only a transparent callee needs
    it; sets the frame predicate twice; has a 0x28 byte stack frame.
    About 20 instructions; `I_LCALL` is 22.5% of an `lr0` loop.  Effort
    S–M.
  - **`I_DEPART`** (`pl-wam.c:2482`): copies the arguments with a
    `movdqu` loop with overlap checks; use `copyWordsScalarUp()` as
    4d95a6a20 did elsewhere.  About 15 instructions for arity 3.
  - **`H_RFUNCTOR`/`B_RFUNCTOR`** (`pl-vmi.c:794`, `pl-vmi.c:1688`): GCC
    turns clearing the arguments into `call memset` even for arity 1–3,
    which also forces a stack frame.  memset is 1.1% of `bench/`.  The
    clearing must stay (GC may run before all arguments are filled);
    use an explicit loop for small arities.  Effort S.
  - **Cold slow paths out of hot VMIs.**  GCC does not shrink-wrap
    `preserve_none` functions, so a call on the slow path gives the fast
    path a stack frame: `B_ATOM`/`H_ATOM` (`markAtom()`), `H_NIL`,
    `B_ARGVAR`, `B_FIRSTVAR`, `H_LIST`, `H_RLIST`, `H_LIST_FF`,
    `H_FUNCTOR`, `S_TRUSTME`, `I_DEPART`.  Move them to cold VMH
    helpers, as 7f98a32f0 did.  2–6 instructions per execution.
  - Smaller: the `LD->profile.active` test in `exit_continue` duplicates
    `ALERT_PROFILE`; `FR->clause = NULL` on deterministic exit may be
    redundant (check against prolog_frame_attribute/3 on stale frame
    references and the GUI debugger); the stack check in `normal_call`
    duplicates the supervisor's `ENSURE_LOCAL_SPACE`.

### 5. Last-call optimisation

`I_DEPART` (`pl-vmi.c:2088`) applies LCO if no choicepoint is newer
than the frame, the `last_call_optimisation` flag is set (debug mode
clears it) and the callee is defined.  The compiler (`lco()`,
`pl-comp.c:3740`) moves arguments in place (`L_*` + `I_TCALL`/`I_LCALL`)
only if all arguments are `B_VAR*`, `B_ATOM`, `B_SMALLINT`, `B_NIL` or
`B_VOID` and no move overwrites a slot that is still needed.  Not
covered: compound, string, bignum and float arguments, fresh variables
(`B_FIRSTVAR`), permuted arguments and module-qualified calls
(`I_DEPARTM`).

LCO saves only about 10 instructions over a plain call (measured): its
value is memory, not speed.  Extending it (swaps, building compounds
in the frame) is effort M for little speed.

### 6. Head code

  - `H_POP` directly before `I_ENTER`/`I_EXITFACT` is 3.2% of all
    dispatched instructions (dynamic count over `bench/`).  Using
    `H_RFUNCTOR`/`H_RLIST` for the last head argument that generates
    code drops it.
  - `H_FIRSTVAR`+`H_POP` is 4.4% and `H_VAR`+`H_POP` 1.2%.  The
    `mergeSeq()` peephole only supports fixed replacement arguments; it
    needs a mode that keeps the argument.

Both about 1–2% (guess).  Risk medium: `skipArgs()`
(`pl-comp.c:5370`) and the head walkers in `pl-index.c` assume that
pushes and pops are balanced.  Effort M.

## Not worth pursuing

  - **Lazy initialisation of variables:** already lazy.  `TRUST_CLAUSE`
    only sets `lTop`; first occurrences write their slot; GC and the
    debugger call `clearUninitialisedVarsFrame()`.
  - **Another argument-passing scheme:** an argument costs about 5
    cycles and dispatch keeps LD, PC, FR, the register file and ARGP in
    machine registers (`preserve_none`, `pl-wam.c:3377-3440`; direct
    threading).  No free register is left for gTop.
  - **Fused `B_VAR` pairs:** inlining the `B_VARn` fast path (4) gets
    most of it more simply.
  - **Inlining more builtins** (functor/3, compare/3, arg/3 beyond
    `B_ARG_CF`/`B_ARG_VF`): all foreign call sites of `bench/` with
    `-O` add up to 0.63M against 236M dispatched instructions.  The
    s(CASP) cost is the work inside functor/3 and unifiable/3, not
    the call.
  - **Fusing `I_ENTER`+`I_CUT`:** 1.5% of dispatches and it removes the
    breakpoint on the cut; at most under `-O`.

## Side findings

  - `'$fetch_vm'/4` crashes if the PC points into the middle of an
    instruction (e.g., at the operand of `I_CALL`).
  - moded_path takes 0.26 s now against 0.10 s in an old `build.pgo`
    (10.1.8, computed goto).  Probably tabling (`clear_variant_table()`
    is 6.7% of the whole suite), not the VM.
  - Clang with tail calls executes about 5% fewer instructions and
    8–12% fewer cycles than GCC in the call micro-benchmarks; on
    `bench/` the runs were too noisy to rank the compilers.

## Suggested order

 1. 1a and 1b: small, clear wins, easy to test.
 2. The call/N cache (2).
 3. Arithmetic by default (3), after deciding on the flag and the
    error behaviour, then the fused comparisons.
 4. The stall-breaking items of 4, measured one at a time.
