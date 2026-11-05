# Why is SWI-Prolog slower than other Prolog systems?

This note compares SWI-Prolog with YAP, SICStus, XSB and Ciao on the
same machine and explains where the difference comes from, down to the
VM instructions.  See `survey.md` for the earlier, more local survey.

Machine: AMD Ryzen AI MAX+ 395 (Zen 5), Fedora 44, GCC 16.2.  All runs
are pinned to one core (`taskset -c 6`).  SWI-Prolog is `build/`
(RelWithDebInfo, VMI_FUNCTIONS, no PGO) with `-O`.  Versions: YAP
6.5.0 (3298a680), SICStus 4 (JIT), XSB, Ciao (from `~/3rdparty`).

## 1. Benchmark suite

`bench/compare.pl swipl-no-pgo yap sicstus xsb ciao` (speedup 1).
Times relative to SWI-Prolog; lower is faster.

| program          |  YAP | SICStus |  XSB | Ciao |
|------------------|-----:|--------:|-----:|-----:|
| boyer            | 0.67 |    0.42 | 1.10 | 0.60 |
| browse           | 0.61 |    0.42 | 0.92 | 0.71 |
| chat_parser      | 0.69 |    0.31 | 0.88 | 0.64 |
| crypt            | 1.34 |    0.45 | 0.74 | 0.58 |
| derive           | 0.61 |    0.31 | 1.08 | 0.47 |
| divide10         | 0.59 |    0.34 | 1.23 | 0.51 |
| eval             | 1.17 |    1.12 | 0.78 | 0.61 |
| fast_mu          | 0.95 |    0.34 | 0.88 | 0.58 |
| flatten          | 0.86 |    0.38 | 1.05 | 0.59 |
| log10            | 0.52 |    0.28 | 0.90 | 0.38 |
| meta_qsort       | 0.56 |    0.30 | 1.06 | 0.54 |
| mu               | 0.50 |    0.32 | 0.79 | 0.54 |
| nand             | 0.76 |    0.42 | 1.04 | 0.74 |
| nreverse         | 0.46 |    0.37 | 0.80 | 0.55 |
| ops8             | 0.65 |    0.32 | 0.96 | 0.46 |
| poly_10          | 0.88 |    0.32 | 1.09 | 0.54 |
| prover           | 0.53 |    0.26 | 0.90 | 0.50 |
| qsort            | 0.64 |    0.29 | 0.89 | 0.48 |
| queens_8         | 1.41 |    0.38 | 0.79 | 0.58 |
| query            | 1.20 |    0.36 | 0.72 | 0.70 |
| reducer          | 0.71 |    0.33 | 1.12 | 0.57 |
| sendmore         | 1.18 |    0.51 | 0.85 | 0.71 |
| serialise        | 0.64 |    0.36 | 0.92 | 0.60 |
| sieve            | 7.14 |    0.95 | 3.14 | 1.44 |
| simple_analyzer  | 0.86 |    0.35 | 1.11 | 0.74 |
| tak              | 1.34 |    0.36 | 0.90 | 0.63 |
| times10          | 0.62 |    0.32 | 1.16 | 0.51 |
| unify            | 0.94 |    0.39 | 1.18 | 0.66 |
| zebra            | 0.46 |    0.43 | 0.81 | 0.68 |
| pingpong (tabling) | 0.29 |     -  | 0.20 | 1.19 |
| moded_path (tabling) | -  |     -  | 0.23 |   -  |
| **geometric mean** | **0.79** | **0.44** | **0.89** | **0.61** |

SICStus compiles to native code (JIT), so it is in a different class.
The relevant comparison is **Ciao and YAP: emulated WAMs that are
1.6x and 1.3x faster**.  XSB, also a WAM emulator, is a little faster
on average.  SWI-Prolog only wins with tabling (vs Ciao) and on
assert/retract-heavy code (`sieve`).

Raw data: `tmp/vm-perf/compare/r1/*.csv`.

## 2. Cost per logical inference

`perf stat -e instructions:u,cycles:u` on the same tiny programs,
minus the startup cost (`tmp/vm-perf/compare/micro/`).

naive reverse of 30 elements, 300,000 times (149M LI):

| system   | instr/LI | cycles/LI | IPC |
|----------|---------:|----------:|----:|
| SWI      |      281 |        60 | 4.7 |
| XSB      |      250 |        48 | 5.1 |
| YAP      |      130 |        23 | 5.6 |
| Ciao     |      124 |        33 | 3.7 |
| SICStus  |       38 |        23 | 1.9 |

`tak(18,12,6,_)` 400 times (51M LI):

| system   | instr/LI | cycles/LI |
|----------|---------:|----------:|
| SWI      |      296 |        57 |
| XSB      |      336 |        65 |
| Ciao     |      154 |        30 |
| SICStus  |       40 |        33 |

**SWI-Prolog executes about twice the machine instructions per
inference of Ciao and YAP.**  A modern wide core hides part of that
(SWI-Prolog's IPC is very high), so the time gap is smaller than the
instruction gap.  The gap is not in a single hot spot; it is spread
over every VM instruction of the clause.

## 3. Where the instructions go: `app/3`

Exact counts from `valgrind --tool=callgrind` (instr/LI; one LI is one
iteration of the second clause of `app/3`):

```
app([H|T], L, [H|R]) :- app(T, L, R).

  S_LIST                     36   clause selection on arg 1
  h_list_ff(3,4)             38   [H|T] in read mode
  h_void                      4   L
  h_list                     12   [H|R] (pushes ARGP)
  H_RLIST (write mode)       47   builds [_|_] for arg 3
  h_var(3)                   28   H (write mode)
  h_firstvar(5)              11   R
  h_pop                      10   pops ARGP
  bvar_cont                   5
  i_enter                     8
  l_nolco                     7
  l_var(0,4), l_var(2,5)     27
  i_tcall                    13
  depart_or_retry_continue   20
                            ---
                            ~270
```

The WAM code for the same clause is

```
  switch_on_term ...
  get_list A1; unify_variable X4; unify_variable A1
  get_list A3; unify_value X4; unify_variable A3
  execute app/3
```

That is 7 instructions with no argument moves at all: `unify_variable A1`
puts `T` directly in the argument register for the next call.

Grouped:

| part                      |  SWI | estimated WAM |
|---------------------------|-----:|--------------:|
| head unification          | ~155 | ~50           |
| argument moves (`l_var`)  |   27 | 0             |
| call, frame, clause select|  ~85 | ~20           |

### 3.1 Head unification

* **Space checks per structure.**  `H_RLIST` (write mode) checks the
  global stack *and* the trail on each list cell it creates;
  `H_LIST_FF` in write mode does the same.  WAMs check heap space once
  per call (or per clause) with a margin.  The compiler knows the
  maximal global usage of a clause head, so one check at clause entry
  suffices.
* **Push/pop for the last argument.**  `h_list` for the third (last)
  argument pushes `ARGP` on the argument stack and `h_pop` restores it,
  although nothing follows.  A compound in the last argument position
  needs no push/pop (WAM: the structure argument is simply the last
  thing processed).  ~20 instr/LI.
* **`UMODE` lives in memory.**  Each head instruction loads it
  (`cmpl $1,(%r15)`) and the write-mode ones store it.  The WAM
  separates read/write mode by code path (`get_list` jumps to a
  write-mode sequence or the instruction sequence is duplicated).
* **Generic write-mode `h_var`.**  28 instructions to store an already
  bound value into a new cell: it reloads the `occurs_check` flag,
  dereferences, distinguishes local/global/attvar and checks trail
  space.
* **Binding checks.**  `bindConst()` tests for attributed variables and
  does the trail test (two compares against the local-stack bounds and
  the choicepoint mark) on every bind.  WAMs do the same trail test,
  but not the attvar test (no attvar) — this one is a feature we keep.

### 3.2 Argument passing

SWI-Prolog keeps arguments in the frame (needed by the debugger to show
the goal at every port).  With last-call optimisation the new arguments
are copied into the current frame by `l_var`.  In the WAM,
`unify_variable A1` puts the value directly where the next call needs
it; the register allocator makes most moves disappear.  For `app/3`
SWI-Prolog could do the same: `T` goes to slot 4 and is then moved to
slot 0.  If the compiler assigned `T` to slot 0 directly (it is free
after `h_list_ff` consumed argument 0), `l_var(0,4)` vanishes.  That is
a register-allocation problem *within the frame*: the debugger only
needs the arguments at the call/redo port; once the head has been
unified, overwriting an argument slot is what LCO does anyway.  But the
debugger's "show the frame of a running clause" would show the new
value for argument 1: this needs a decision (this is what `-O` could
allow, just as it allows LCO).

### 3.3 Calls

Per call SWI-Prolog does, besides selecting the clause:

* `i_enter` (8): marks the body start.
* `l_nolco` (7): checks whether LCO is allowed (debug mode, a choice
  point in the frame).
* `i_tcall` (13): increments `level`, clears frame flags, resets
  `FR->clause`, checks `HIDE_CHILDS`.
* `depart_or_retry_continue` (20): stores the generation, the profiler
  node, increments `inferences`, checks `LD->alerted`.
* `S_LIST` (36): selects the clause, checks local stack space, sets
  `lTop`, resets `UMODE`, stores the clause.

A WAM `execute` is a jump plus storing the predicate address.  The
SWI-Prolog frame carries information that the GUI debugger, the
profiler, the logical update view and GC need (level, generation,
clause, profiler node, flags).  Several of these can be cheaper:

* `prof_node` only needs storing when profiling (`LD->alerted` is
  already tested and covers profiling).
* The generation only needs storing when the predicate is dynamic
  (or is being reloaded): static code that is not being reloaded has
  all clauses visible.
* `i_enter` + `l_nolco` can be merged into one instruction for the
  common case (no LCO blockers decided at compile time with `-O`).
* The local stack check in `S_LIST` could use a margin checked once
  per call, as for the global stack.

## 4. Where the instructions go: `tak/4`

```
tak(X,Y,Z,A) :- X =< Y, !, Z = A.
tak(X,Y,Z,A) :- X1 is X-1, ..., tak(A1,A2,A3,A).
```

Here the clause choice is unindexable, so each call creates a
choicepoint, the first clause usually fails, and otherwise cuts.

| part                                             | instr/LI |
|--------------------------------------------------|---------:|
| `S_STATIC` + `firstClause` + `first_clause_guarded` + `next_clause_unindexed` + `nextClause` | ~60 |
| cut: `I_CUT` + `discardChoicesAfter`             |       35 |
| `Z = A` (fresh var): `B_UNIFY_VV` + `unify_ptrs` |       40 |
| `X =< Y` (`A_ENTER`, `a_var_n` x2, `A_LE`, compare)  |  ~55 |
| `is/2` x3 (`A_ADD_FC`)                            |       13 |
| backtrack to clause 2 (`shallow_backtrack`)       |       11 |

* **Clause selection is generic C.**  For a static predicate with 2
  unindexable clauses, the WAM does `try_me_else`/`trust_me` (≈15
  instructions).  SWI-Prolog calls `firstClause()`, which consults the
  index information, then `first_clause_guarded()` and
  `next_clause_unindexed()` which check the visibility (generation) of
  each clause.  A supervisor for "static, ≤ N clauses, no index" that
  walks the clause list without generation checks (as `S_LIST` does
  for lists) would cut most of this.
* **Cut is generic.**  `discardChoicesAfter()` loops over all frames
  between the choicepoint and the cut frame, checking each for
  `FR_WATCHED` (debugger/cleanup).  For the common case — the
  choicepoint created by this frame's own clause selection, nothing
  watched in between — cut is `BFR = BFR->parent`.
* **`Z = A` with `A` fresh** goes through `unify_ptrs()`.  A fast path
  for var-var and var-nonvar in the VMI avoids the call.
* **Arithmetic comparison** pushes both operands on the arithmetic
  stack (`a_var_n`, 13.5 instructions per operand), `A_LE` compares
  them (it has a small-integer fast path, `CMP_FAST`), and `A_ENTER`
  and `a_cmp_out` set up and pop the arithmetic stack: ~55 instructions
  for `X =< Y`.  A compare-two-variables instruction (`A_LE_VV` etc.)
  that reads both frame slots directly and only falls back to the
  arithmetic stack if they are not both small integers would cost ~10.

## 5. What keeps SWI-Prolog's good parts

Things that are *not* the problem, or that we keep:

* Dispatch: the VMI_FUNCTIONS tail-calling VM dispatches with one
  indirect jump per instruction; `l_var` is 13 machine instructions,
  close to minimal.
* Attributed variables, the logical update view, unbounded integers,
  GC, the debugger's frames.  These cost a few tests per operation;
  the bulk of the gap is elsewhere.

## 6. Candidates, ordered by estimated gain per effort

| # | change | est. gain (instr/LI) | risk |
|---|--------|---------------------:|------|
| A | One global+trail space check per clause head instead of per structure (compiler computes the maximal need) | nrev 15–25 | GC/shift interaction: the margin must be guaranteed for GC too |
| B | No push/pop for a compound in the last argument position (head and body) | nrev ~20 | low: compiler-only |
| C | Fast cut: own choicepoint, nothing watched → `BFR = parent` | tak ~25 | low |
| D | Supervisor for small static unindexable predicates: no `firstClause()` call, no generation checks when the predicate is not being reloaded | tak ~40 | medium: reload/LUV correctness |
| E | Frame bookkeeping: store `prof_node` and generation only when needed | all ~5–10 | medium |
| F | Fast paths in `B_UNIFY_VV` (var-var/var-nonvar) and compare-two-variables instructions (`A_LE_VV`, …) for small integers | tak ~60 | low |
| G | Allocate head variables to the argument slot they will be passed in (LCO), avoiding `l_var` moves; `-O` only | nrev ~15 | debugger shows changed arguments for running frames |
| H | Split read/write mode by code path instead of the `UMODE` variable | all, ~1–3 per head instruction | large change |

A+B+C+F are low risk and together would bring nrev from ~280 to ~220
instr/LI and tak from ~300 to ~200: halfway to Ciao.  Getting to the
Ciao/YAP level needs the call path (D, E, G) and probably H.
