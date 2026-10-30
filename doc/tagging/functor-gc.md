# Garbage collecting functors

Status: **design, not implemented.**

Atoms are garbage collected; functors are not.  This note looks at the
options for collecting both, including merging atoms and functors into
one kind of object, and recommends the cheapest one.  It is
independent of the tagging redesign in [top-tags.md](top-tags.md).


## The problem

A functor never dies, and it keeps its name atom alive forever:
`lookupFunctorDef()` (`pl-funct.c`) calls `PL_register_atom()` on the name
and nothing ever drops that reference.  Measured on master (e8b9d0d3d):

```
100k  T =.. [f<I>, x]  + AGC:   functors 4592 -> 104592,  atoms 7863 -> 106776
100k  plain atoms      + AGC:   atoms 106776 -> 106778   (collected)
```

Plain atoms are collected, but every atom that ever named a compound
leaks, together with its functor.  The cost is a `struct functorDef` (40
bytes) plus table slots, and the atom with its text.  Any program
that builds compounds from data leaks without bound: `=../2`,
`functor/3`, `compound_name_arity/3`, or `read/1` on input from a socket
or a pengine client.  For a server that is a denial-of-service vector.


## Why atoms and functors cannot simply merge

Some systems treat an atom as a functor with arity 0 and keep a single
dictionary of name/arity pairs (e.g. ECLiPSe).  SWI-Prolog has
compounds with zero arguments (`foo()`), so `foo/0` is a real functor
and is distinct from the atom `foo`.  A merged table therefore needs a
separate _atom_ arity marker.


## Options

### 1. Collect functors in AGC; keep two kinds of object

  - **Stacks:** AGC already walks the global stack of every thread
    (`markAtomsOnGlobalStack()`, `pl-gc.c`).  `isAtom()` uses `tagex()`
    precisely to skip functor cells there.  Marking functors adds one
    `isFunctor()` branch.  The asynchronous scan, the write barrier and
    the deferred free apply to functors unchanged.
  - **Other references:** reference counts wherever atoms get them now:
    - clauses: a `forFunctorsInClause()` next to `forAtomsInClause()`
      (`pl-comp.c`), walking the functor arguments of the VM code;
    - records (`pl-rec.c`) and record keys;
    - trie keys (`pl-trie.c`);
    - procedures and module procedure tables (`pl-proc.c`);
    - global variables (`pl-gvar.c`).
  - **Freeing:** a functor that is unmarked and has no references is
    freed.  This releases its name atom, so the atom can go in the
    same or a later AGC.
  - **Index reuse:** a free list, as atoms have.  Tables indexed by
    functor number hold only built-in functors (e.g. the arithmetic
    function table), which are permanent.
  - **Lookup racing collection:** `lookupFunctorDef()` may find a
    functor that AGC is freeing.  This is the same race as for atoms,
    solved with `VALID_F` and the reference count.
  - **Foreign code:** C code caches `functor_t` values in static
    variables without registering them.  Unlike atoms, functors have no
    register/unregister contract.  Hence `PL_new_functor()` keeps
    returning a permanent functor, and a new `PL_unregister_functor()`
    lets code opt in.  Only functors created by Prolog code become
    collectable, which is where the leak is.  `FUNCTOR_*` constants of
    the core stay permanent.
  - **Semantics:** `current_functor/2` may see functors disappear, as
    `current_atom/1` already does.

Earns: bounded memory for the data-driven case, and the name atoms are
collected too.

Costs: moderate work, no change to the representation, an extension
to the foreign API but no break, and negligible run-time overhead
(reference counts on clause add/erase, one more test in the stack
scan).

### 2. Functors as properties of their atom

This is the YAP approach: each atom carries a short chain of its
functors, keyed by arity, and the global functor hash table goes away.
`functor_t` stays a compact index because cells need room for the
inline arity.

Earns:

  - One liveness decision per symbol: an atom is live if it is
    referenced or any of its functors is.
  - Lookup becomes atom → small chain, with no global table, no rehash
    and no compare-and-swap on shared buckets.  That helps `=../2`,
    `functor/3` and `read/1`.

Costs: one pointer per atom, including blobs, which never get
functors, and a concurrent per-atom chain.

This is fully internal and can follow option 1 as a lookup
optimisation.  It is not needed for GC.

### 3. One symbol table, an atom is "arity ATOM"

One entry per name/arity pair, with a marker for the atom itself.

Earns: one table, one collector, one index space.

Costs:

  - `f`, `f/0` and `f/2` must still share the text.  A functor entry
    therefore still points to a name object, so there are still two
    kinds of object; only the handle space is merged.
  - Every `atom_t` value changes, including all generated `ATOM_*`
    constants.
  - Blobs (streams, clause references, etc.) carry fields they never
    use.
  - The foreign API keeps `atom_t` and `functor_t` as separate types
    anyway.
  - Cells still need separate tags for atoms and functors: GC and AGC
    must recognise the functor cell that starts a compound.  That
    holds for today's storage bits and for `T_ATOM`/`T_FUNCTOR` in
    [top-tags.md](top-tags.md).

Option 3 earns little beyond option 1 for a large disruption.


## Recommendation

Implement option 1.  It removes the leak, it is independent of the
tagging redesign, and it reuses the AGC machinery.  Consider option 2
only if profiling shows functor lookup or rehashing as hot.  Drop
option 3.


## Plan

 1. Add a regression test: build 100k compounds with fresh names, run
    AGC, and check that `statistics(functors, N)` and
    `statistics(atoms, N)` return to near their start values.
 2. Reference counts on functors from clauses, records, tries,
    procedures and global variables; `PL_unregister_functor()`.
 3. Mark functor cells in `markAtomsOnGlobalStack()`; free unmarked,
    unreferenced functors in AGC, releasing their name atoms; free-list
    reuse of functor indexes.
 4. Documentation: foreign interface (`PL_new_functor()` is
    permanent unless unregistered) and `current_functor/2`.
