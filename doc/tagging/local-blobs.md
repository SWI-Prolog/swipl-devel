# Thread-local blobs

Status: **design.**  Steps 1–4 of the [plan](#plan) are done, as are
local unique blobs and their first users, clause and record references
(step 5).

Many C objects that Prolog refers to are used by one thread only:
tabling worklists and SCC components, private answer tries, Janus
Python objects, most streams and clause references.  Now and then such
a reference leaves the thread: another thread inspects a table (IDG
viewer), the reference is stored in the database or sent in a message.
This note proposes blobs that start life owned by their thread, are
reclaimed by that thread without global atom-GC (AGC), and are
_promoted_ to ordinary shared blobs when they escape.  Promotion does
not change the `atom_t`, so there is only ever one representation of a
reference.


## The problem

| object | handed to Prolog as | issue |
|--------|---------------------|-------|
| worklist, SCC component (`pl-tabling.c`) | raw integer (`PL_unify_pointer()`) | `get_worklist()` only `assert()`s the magic; `get_scc()` reads the magic of possibly freed memory.  A stale integer is a use-after-free. |
| private answer trie | trie blob, created lazily by `trie_symbol()` | each private table that reaches Prolog costs an atom that only AGC reclaims |
| answer trie node | raw integer | unchecked |
| Janus Python object | blob | created in bulk, mostly temporary: AGC pressure |

Tables can be abolished at any time, also while the thread is
backtracking into them, and other threads may inspect them.

## What blobs cost today

  - A `PL_BLOB_NOCOPY` blob without `PL_BLOB_UNIQUE` (e.g. the trie
    blob) is not in the atom table.  `lookupBlob()` takes a slot from
    the thread's reserved batch (`reserveAtom()`) and fills it in.
    Creation is cheap.
  - `PL_BLOB_UNIQUE|PL_BLOB_NOCOPY` hashes on the `->name` pointer, so
    "the blob for this C object" is a lock-free hash lookup.  This is
    what clause references could use.
  - Every creation does `ATOMIC_INC(&GD->statistics.atoms)`, a cache line
    shared by all threads, and every 128 creations `considerAGC()`.
  - Reclaiming a blob needs a global AGC: all threads are scanned.
  - Since Janus, a blob can be _discarded_: `PL_free_blob()` calls the
    `release` hook and sets `->name` to `NULL`.  The atom lives on, but
    `PL_blob_data()` returns `NULL`, so a stale reference raises an
    existence error rather than reaching freed memory.  Object lifetime
    and atom lifetime are thus independent.


## Design

### State

A new state bit `ATOM_LOCAL_REFERENCE` in `Atom->references`, next to
`ATOM_RESERVED`/`VALID`/`MARKED`/`DESTROY`.  Local atoms live in aligned
chunks of 64 atom slots owned by one thread; a global table maps a chunk
to its owner and its `used` and `marked` bitmaps (see step 1 of the
plan).  Each thread counts its live local atoms (`LD->atoms.local`).
If that count is 0, all escape checks below are skipped.

### Creation

`lookupBlob()` with a flag (or a blob-type flag `PL_BLOB_LOCAL`) that
sets `ATOM_LOCAL_REFERENCE` and counts in `LD->atoms.local` instead of
`GD->statistics.atoms`.  `PL_BLOB_UNIQUE` blobs go into the atom table
as now, so that a lookup for the same C object finds them, also from
another thread (see promotion).  Objects without a unique blob type
store their atom themselves (`trie->symbol`, `wl->symbol`, ...).

### Roots of a local atom

While an atom is local, only its owner can reach it, from:

  - the owner's stacks;
  - the owner's findall bags (`markAtomsFindall(ld)`);
  - counted references from storage of the owner thread;
  - `ld->atoms.unregistering`.

### Promotion (escape)

A local atom becomes shared by clearing `ATOM_LOCAL_REFERENCE` with the
CAS on `references` that the operation does anyway.  From then on it is
an ordinary blob, reclaimed by AGC.  Escapes:

  - **Registration.**  `PL_register_atom()` (clauses, records, trie keys
    that register, global data) promotes.  Thread-local containers may
    instead use a local registration that counts without promoting.
  - **Unique lookup from another thread.**  `lookupBlob()` finds the
    local atom of another owner: `bump_atom_references()` promotes.
  - **Object accessors in another thread.**  E.g. `trie_symbol()` called
    by a thread that follows an IDG edge into the tables of another
    thread.  It promotes the owner's symbol with a CAS that races with
    the owner's sweep, or installs a new, shared atom with a CAS on
    `obj->symbol` (see step 4).
  - **Unregistered copies.**  `R_NOLOCK` records in message queues and the
    keys of trie chunks are marked by AGC rather than registered.
    Sending a message and inserting into a shared trie promote the local
    atoms they copy.  Both already walk the term, so this is a test per
    atom, and none at all if `LD->atoms.local == 0`.

Over-promotion is never unsafe, it only delays reclamation to the next
global AGC.  Under-promotion is a bug: a local atom reachable from
another thread may be freed by its owner.  A debug check in
`PL_unify_atom()`/`PL_get_blob()` that the caller owns any local atom it
sees catches these.

### Local sweep

When `LD->atoms.local` grows past a threshold, which scales with the
stack size as the GC thresholds do, the owner sweeps at a safe point:

 1. Mark the local atoms reachable from its roots, using a local mark
    bit.  The stacks are scanned with `markAtomsOnStacks()` in a new
    _safe_ mode: `markAtomsOnGlobalStack(ld, safe)` trusts the indirect
    headers instead of bounding them, and the local stack scan needs no
    margin above `lTop`.  At a safe point this scan is complete.
 2. Release the unmarked local atoms that have no counted references:
    call the `release` hook and return the slot to the thread's
    reserved batch.

No other thread is involved.  Global AGC skips atoms that are still
local.  When a thread terminates, its local atoms are released.

As no other thread sweeps them, a thread or engine that does not run
keeps its dead local atoms, and so the resources of their blobs.  A
Prolog thread blocked in `thread_get_message/1` with 53,000 dead JPL
references kept them all, where global AGC used to release them.
Therefore an idle thread or engine sweeps if it created local atoms
since its last sweep (`idleLocalSweep()`):

  - `thread_idle/2`, after its stack trimming or GC.  Servers wait
    for the next request using this, e.g., the HTTP workers and
    pengines.  The thread above keeps 15 references.
  - `PL_set_engine()` detaching an engine without open queries, as a
    JPL query does when it returns its engine to the pool.  The stacks
    are empty, so the sweep is cheap.

### Object lifetime: discard

Discarding decouples freeing the C object from reclaiming its atom:

  - Freeing a worklist or component discards its blob: `->name`
    becomes `NULL`.  References to it elsewhere get an existence error.
    Answer tries do not use this: an abolished table hands its trie to
    the symbol, if any, so stale handles keep referring to an empty
    trie as before (step 4).
  - C code that keeps a raw pointer across calls must hold a reference
    to the object itself, as `trie_gen` already does (`acquire_trie()`).
    The object is freed when its reference count drops to 0.  Discarding
    only drops the owner's reference.
  - An inspector in another thread uses the same object reference while
    it walks the table.

The atom slot is then reclaimed lazily, by the local sweep or by AGC.
For objects whose lifetime is owned by Prolog (Janus objects, streams
without an explicit close) the `release` hook of the sweep frees them.


## Users

  - **Worklists and SCC components.**  Local blobs instead of raw
    integers.  They are never promoted in practice.
  - **Private answer tries.**  Local symbols; promoted when inspected
    from another thread or stored.  After abolish, the symbol owns the
    trie.
  - **Janus.**  Python object references are created in bulk and mostly
    temporary.  The sweep runs in the owner thread, which simplifies
    acquiring the GIL in the `release` hook.
  - **JPL.**  Java object references and Strings returned by Java.
    The release hook deletes a JNI global reference in the owner
    thread, which is attached to the JVM.
  - **Streams.**  Usually opened and closed by one thread.  An alias or
    `stream_property/2` from another thread promotes.
  - **Clause and record references.**  `PL_BLOB_UNIQUE|PL_BLOB_NOCOPY` on
    the `Clause` or `RecordRef`: a lookup from another thread finds and
    promotes the local atom.  References kept in shared C data use
    `lookup_shared_clref()` (coverage data, the clause of a compiled
    trie).
  - **Not:** answer trie nodes.  There are too many; they stay integers
    or are validated through their trie.


## Related: precise AGC at safe points

Global AGC marks the stacks of running threads asynchronously.  This is
conservative and needs `pushVolatileAtom()` for atoms created during
the scan.  An alternative is to let threads that ask for precise AGC do
their own marking at a safe point, as for `thread_signal/2`, using the
same safe mode of `markAtomsOnStacks()`.  With local blobs, most
short-lived blobs never reach global AGC, so this matters less.  The
safe mode is needed for the local sweep anyway.


## Costs and risks

  - Escape paths that bypass registration and the paths listed above.
    This needs an audit of C code that passes an `atom_t` to another
    thread or reads another thread's data.
  - `current_blob/2` and the atom statistics must skip, or promote, the
    local atoms of other threads.
  - The local sweep is linear in the stack size.  The threshold must
    make it rare relative to the number of local atoms created.
  - The chunk table costs one 32-byte descriptor per 64 atom slots that
    were ever used for local atoms.  Chunk slots are reserved for local
    atoms, also while the chunk is in the pool, so ordinary atoms do not
    reuse them.


## Plan

 1. **Done.**  `markAtomsOnStacks()` safe mode; local sweep machinery
    with the `ATOM_LOCAL_REFERENCE` state, without promotion:
      - `ATOM_LOCAL_REFERENCE` takes one bit of the reference count,
        which now has 27 bits (26 since step 4).
      - Blob types with the internal flag `PL_BLOB_LOCAL` (not yet in
        `SWI-Prolog.h`) create local atoms.  Such types cannot be
        `PL_BLOB_UNIQUE` yet.
      - Local atoms live in chunks of 64 consecutive, aligned atom slots.
        `GD->atoms.local_chunks` maps a chunk number to a descriptor with
        the owner and the `avail`, `used` and `marked` bitmaps; only the
        owner writes the bitmaps and descriptors are never freed.  A
        thread takes a chunk from a global pool of returned chunks or by
        extending the atom array to the next aligned chunk; the skipped
        slots become holes for ordinary atoms.  Creating a local atom
        takes the lowest free bit of a chunk (`reserveLocalAtom()`).
      - Marking (`markLocalAtom()`) sets a bit in the chunk's `marked`
        bitmap if the calling thread owns the chunk: a plain write to
        thread-owned memory.  Global AGC neither marks nor collects local
        atoms.
      - The sweep (`collectLocalAtoms()`) is requested through the
        existing `SIG_ATOM_GC` signal of the owner, not a new signal, and
        also runs from `garbage_collect_atoms/0`.  Roots are the stacks,
        `ld->atoms.unregistering`, the findall bags and the thread's
        message queue.  `markAtomsRecord()`, `markAtomsFindall()` and
        `markAtomsThreadMessageQueue()` take the mark function.
      - A sweep costs a scan of the stacks plus a visit of every used
        slot, so it only runs if there is at least one live local atom per
        16 words of stack (`considerLocalSweep()`).  If a gc thread does
        global AGC, `agc_handler()` in other threads only does the local
        sweep (`agcByGCThread()`); before, reusing `SIG_ATOM_GC` made the
        tabling thread run global AGC itself.
      - Local `PL_BLOB_NOCOPY` blobs are created by `lookupLocalBlob()` or,
        without a reference, `newLocalBlob()`: just a slot, no atom table,
        no MurmurHash.  Local atoms are not counted in the `live`/`space`
        statistics of their type, avoiding atomic updates of shared
        memory.
      - The sweep releases `used & ~marked` atoms without references.
        Afterwards, empty chunks go to the pool as long as the thread
        keeps at least its new threshold of slots.
      - `current_blob/2` only enumerates local atoms in their owner, and
        they are not counted in `GD->statistics.atoms` or towards the
        AGC margin.
      - When a thread terminates, its local atoms without references are
        released; the others become ordinary atoms.  Its chunks go to the
        pool, without the slots of the promoted atoms.
      - Tests: `tests/GC/test_local_blobs.pl`, using
        `'$local_test_blob'/1` and `'$local_atoms'/2`.
 2. **Done.**  Worklists and SCC components as local blobs (no promotion
    needed):
      - Blob types `worklist` and `table_component`
        (`PL_BLOB_LOCAL|PL_BLOB_OWNED|PL_BLOB_NOCOPY`).  The object creates
        its blob on demand (`newLocalBlob()`) and remembers it in
        `symbol`, but holds no reference: the blob lives as long as Prolog
        refers to it.  The release hook, run by the owner's sweep, clears
        `symbol`; a later request creates a new blob.  Handles that exist
        at the same time are the same atom.  For promotion, see step 3.
      - A first version let the object hold a counted reference.  In
        ErgoAI cyc150 that kept millions of blobs alive for the lifetime
        of large SCCs, and after completion they were garbage that no
        sweep reclaimed until the `live` threshold, which had grown with
        them, was reached again.  Triggering sweeps on dropped references
        made it worse (80 s instead of 30 s): each sweep visits every used
        slot.
      - Freeing the object discards the blob (`discardBlob()`: the data
        becomes `NULL`).  A stale handle raises an existence error;
        before, it reached freed memory or a recycled object
        (`tbl_alloc()` reuses freed worklists and components).
      - Worklists and components record their `owner`.  For another
        thread, `unify_table_status()` reports a shared table under
        completion as `incomplete` instead of returning the owner's
        worklist, so handles never leave their thread.
      - Suspensions keep worklist handles in fastheap terms, which
        register their atoms.  The live SDG decodes the caller worklist
        of a suspension as a blob.
      - Answer trie nodes and the other raw pointers of `pl-tabling.c`
        remain integers.
      - Memory: in cyc150 the handles need about 2M extra atom slots at the
        peak.  The base's atom array is dense (6.45M slots for 6.44M
        atoms), so the slots extend it past 2^23 and `allocateAtomBlock()`
        allocates and initialises a block of 8.4M slots (about 400 MB),
        almost all unused.  Since "Do not initialise new blocks of the atom
        array", new atom blocks are mapped,
        untouched zeroed memory, which brings the peak to 9.97 GB (base
        9.83 GB) and the solve time to 29.7 s (base 28.2–28.8 s).
      - Cost, measured 2026-10-30 against the parent commit on a quiet
        machine (pinned, best of 5): the tabling benchmarks take 101% in
        total (bintree18 103%, 10kfib 108%); ErgoAI cyc150
        (`diploidnumberfordogs-2`) solves in 30.0–30.3 s instead of
        28.5–28.6 s (+5.5%) with a peak of 10.34 GB instead of 9.83 GB.
        Each fresh table creates two blobs (component and worklist).
      - Tests: `tests/tabling/test_tbl_handles.pl`.
 3. **Done.**  Promotion (`promoteLocalAtom()`): clear
    `ATOM_LOCAL_REFERENCE`, account the atom globally and, if global AGC
    is active, mark it.  The slot stays in the owner's `used` bitmap
    until its sweep finds the atom is no longer local.
      - `PL_register_atom()` and `bump_atom_references()` promote in the
        CAS that adds the reference.  `registerAtomLocally()` adds a
        reference without promoting, for data of the owner thread.  Users:
        global variables (`nb_setval/2` and friends are thread-local) and
        `term_to_fastheap_ex(t, true)`, which tabling's suspensions use.
        Event hooks (`prolog_listen/2`) and IDG monotonic dependencies
        keep promoting fastheap terms.
      - Still promoting although the data is thread-local, so only
        over-promotion: clauses of `thread_local/1` predicates and the
        registered keys of private tries (`acquire_key()`).  Step 4 should
        revisit the latter.
      - Unregistered copies promote: keys of chunked trie nodes
        (`mark_new_key()`; only private tries use chunks) and the records
        of message queues (`create_thread_message()`).
      - In `O_DEBUG` builds with `validate_api`, `check_atom_t()` raises
        an API error for a local atom of another thread.  Doing this for
        `validate_api` alone, which is on by default, cost about 3% in
        ErgoAI cyc150 (about 67 instructions per table).
      - `promoteAtomIfLocal()` first tests the owner of the atom's chunk:
        the chunk table is far denser than the atom array.
      - Cost: ErgoAI cyc150 solves in 30.4–30.8 s, the same as after
        step 2 (29.8–30.7 s); the base takes 28.5–28.9 s.
      - Normally the owner promotes: until it stores the atom where other
        threads can reach it, nobody else can see it.  The exception is
        a thread that inspects the tables of another thread and takes a
        trie handle (step 4).
      - `PL_BLOB_OWNED` blobs belong to a C object that remembers them,
        such as a tabling worklist.  Promotion adds a reference for the
        object in the same CAS, which the object drops when it is freed
        and discards the blob.  Global AGC thus never reclaims such a
        blob while the object lives, the object's `symbol` stays valid
        (a promoted handle is still `==` to later handles), and the
        release hook only runs in the owner's sweep.  A first version
        without this had to keep the hook from running in other threads,
        which cannot be decided safely, and gave promoted handles a new
        identity.
      - Object accessors from another thread and unique lookups remain
        for steps 4 and 5: no local blob type uses them yet.
      - Tests: `promote_assert`, `promote_message` and `promote_answer`
        in `tests/GC/test_local_blobs.pl`, using `'$local_atom'/1`;
        `gvar_local` there; and `promoted_identity` in
        `tests/tabling/test_tbl_handles.pl`, which promotes with
        `recorda/3` and drops that reference with `erase/1`.
        Whether a sweep reclaims an unpromoted trie key depends on stale
        words found by the conservative stack scan, so `promote_answer`
        tests the promotion itself.
 4. **Done.**  Private answer tries have local symbols:
      - Private answer tries get `TRIE_LOCAL`.  The `trie` blob type is
        now `PL_BLOB_LOCAL|PL_BLOB_OWNED|PL_BLOB_NOCOPY`; the symbols of
        other tries (shared tables, `trie_new/1`, variant tables) are
        created shared (`newSharedBlob()`), holding the reference of the
        trie's owner as before.  A second blob type for local tries would
        make `blob_type_property/2` list `trie` twice.
      - The variant table node owns a private answer trie.  Its local
        symbol has no reference for the trie, so the owner's sweep
        reclaims it like a worklist handle: the release hook clears
        `trie->symbol` and a later request creates a new symbol.  Before,
        each table that reached Prolog had a global atom, owned by the
        node, that only AGC reclaimed after the table was abolished.
      - Abolish does not discard.  The node hands the trie to its symbol
        (`TRIE_ORPHANED`, `release_local_trie()`), which destroys it when
        reclaimed: by the owner's sweep if it is still local, by AGC
        otherwise.  Stale handles thus still refer to an empty trie, as
        before.  Without a symbol, the trie is destroyed immediately.
      - Handing over must not race with a promotion by another thread,
        which would add the node's reference after the node dropped it.
        `disownBlob()` sets the new state bit `ATOM_DISOWNED_REFERENCE`
        with a CAS if the atom is still local, after which promotion adds
        no reference for the object, and else drops the object's
        reference.  This also fixes a leak of step 3: an owner that
        promoted a discarded worklist handle, e.g., by asserting it,
        added a reference nobody dropped.  `discard_tbl_handle()` now uses
        it as well.  The bit takes one more bit of the reference count.
      - Another thread (`local_trie_symbol()`, owner test:
        `trie->alloc_pool == LD->tabling.node_pool`) promotes the
        owner's symbol (`shareLocalBlob()`, which validates type and data
        and races with the owner's sweep through the CAS on the
        references) or installs a shared one with a CAS on
        `trie->symbol`.  It then checks `trie->symbol` again: the owner
        may have reclaimed the atom and reused its slot, in which case we
        may have promoted an unrelated atom, which is safe.  A destroyed
        trie gets `ATOM_nil` as symbol with a CAS, so no other thread
        installs a symbol for it; such a thread gets a discarded blob.
      - Over-promotion avoided: `droppable_table()` (components) and
        `tt_add_table()` (transactions) register locally.
        `'$idg_edge'/3` and friends kept the symbol of the next
        dependent in their foreign state across a redo without a
        reference; they now keep the trie and ask its symbol when they
        unify it.
      - The keys of private tries still promote (`acquire_key()`,
        `mark_new_key()`): the sweep does not scan tries for roots.
      - The local stack scan keeps its margin above `lTop`: at a safe
        point in a call, the arguments of the frame being built are
        there.  It finds stale words of earlier frames; tests that need
        a symbol to be reclaimed first overwrite that area.
      - Cost, measured 2026-10-30 against step 3 (quiet machine): the
        tabling benchmarks take 100.0% in total, none differs by more
        than 4%; ErgoAI cyc150 solves in 29.5–30.4 s instead of
        30.2–30.8 s, with a peak of 9.66 GB instead of 9.97 GB (the base
        before step 1: 9.83 GB).
      - Tests (`tests/tabling/test_tbl_handles.pl`): `trie_local`,
        `trie_no_global_atoms`, `trie_reclaimed`, `trie_abolished`,
        `trie_promoted`, `trie_disowned`, `disowned_worklist` and, for
        another thread taking a handle through the IDG edge of a shared
        incremental table, `trie_steal_existing` and `trie_steal_new`.
        Each detects a mutation of the code it tests.
 5. Other blob types.  Most of these are `PL_BLOB_UNIQUE`, so this
    needs local unique blobs first: the sweep must take them out of the
    atom table, using the bucket protocol of `invalidateAtom()` and
    `destroyAtom()`.  `PL_BLOB_LOCAL` then moves to `SWI-Prolog.h`.
    First for clause and record references, which were copying
    `PL_BLOB_UNIQUE` blobs (a `clause_ref` or `RecordRef*` as key, with
    a malloc() per blob and a hash over the copy):
      - **Done.**  They are now `PL_BLOB_UNIQUE|PL_BLOB_NOCOPY` on the
        `Clause` or `RecordRef` itself, ordered by address (a compare
        hook: for `NOCOPY` blobs, standard order otherwise compares the
        bytes of the clause, which change).  The VM used the copied
        `clause_ref` as frame clause: a restarted continuation now
        pushes a `clause_ref` below its frame, where AGC finds the blob
        in its key and the stack shifter relocates the frame's
        `clause`; a compiled trie clause carries a `clause_ref` after its
        code (`trie_clause_cref()`).  Measured against the previous
        commit: `clause/3` enumeration 10–28% faster, `recorded/3` 8%,
        `assertz/2`+`erase/1` and `reset/3` 3–5%; tabling benchmarks
        98.5% in total; ErgoAI cyc150 unchanged.
      - **Done.**  Local unique blobs, used by clause and record
        references.  They are in the atom table, so a lookup from
        another thread finds them; it promotes, a lookup by the owner
        only adds a reference.  The owner's sweep releases them in a
        second pass under `L_REHASH_ATOMS`, using the protocol of global
        AGC: the release hook, then unlink from the bucket, then keep
        the slot until no lookup is in that bucket.  It takes the lock
        with a trylock, as global AGC holds it while marking; if that
        fails, they wait for the next sweep.  If a lookup is in the
        bucket, the atom goes to global AGC's `invalid_atoms` and its
        slot leaves the chunk.  Thread exit waits for the lock.
        Rehashing counts local atoms in the table
        (`GD->atoms.local_unique`).
          - `promoting_ref()` takes the type from the caller: a lookup
            may hold an outdated `references` word of an atom that the
            sweep has unlinked, which overwrites `type` with
            `next_invalid`.  Found as a crash with 4 threads enumerating
            `clause/3`.
          - The counter is in `GD`, so `PL_cleanup()` resets it.  A
            first measurement suggested that adding it made
            `accVariantsComp` 8% slower by shifting the rest of `GD`;
            rerunning static against `GD` gave equal times (within 1%).
            `perf mem` (AMD IBS) shows why `GD` layout does not matter
            much: on `accVariantsComp` 0.2% of the loads and stores hit
            `GD`, on ErgoAI 1.4% and on 4 threads enumerating
            `clause/3` 3.3%, with almost no L1 misses (34 of 9,789
            samples on ErgoAI, 2 of 1,218 with 4 threads).
          - Cost, measured 2026-10-31 against the previous commit: the
            tabling benchmarks take 98.9–99.8% in total (single
            benchmarks ±7%, in both directions over reruns).  Single
            threaded `clause/3` enumeration with references is ~2%
            faster, `recorded/3`, `assertz/2`+`erase/1` and `reset/3`
            are unchanged.  Four threads each enumerating 100,000
            clause references 20 times: 0.57–0.61 s instead of
            0.61–0.78 s, with 56–68 instead of 252–285 global AGC runs.
            ErgoAI cyc150 solves in 29.9–30.2 s instead of 29.3–29.4 s,
            same peak (9.66 GB).  The extra samples are in functions
            whose object code is identical in both builds
            (`S_INCR_DYNAMIC`, `idg_set_add`); `pl-atom.o` grew by
            2.5 KB, which moves the code that is linked after it.
          - Tests (`tests/db/test_dbref.pl`): `local_clause`,
            `local_record`, `shared_identity`, `local_reclaimed`,
            `local_slots_reused` and `promoted_kept`.  Each detects a
            mutation of the code it tests.  The hand-off to global AGC
            when a lookup is in the bucket has no test: it needs a
            lookup to be in the bucket at the moment of the sweep.
      - **Done.**  `PL_BLOB_LOCAL` is public (`SWI-Prolog.h`, documented
        with the blob flags in the manual), so foreign blob types such
        as xpce's `pce_blob` and Janus' `PY_OBJECT` can use it.  These
        reference external objects and use `gc_margin` to express what
        they keep alive.  Local atoms are not in the type's global
        accounting, so a thread now counts the `len` of the local blobs
        it creates of types with a margin (`LD->atoms.local.units`).
        When the units created since its last sweep reach the margin it
        raises `SIG_ATOM_GC` and sweeps, regardless of the stack scan
        ratio.  The sweep recounts the units of the survivors
        (`units_kept`), so live blobs do not cause a sweep per new blob.
        The units of all types with a margin are counted together.
        Tests (`tests/GC/test_local_blobs.pl`, unit `local_margin`, on
        `'$local_unique_test_blob'/2`, a `UNIQUE|NOCOPY|LOCAL` blob on a
        malloc()ed object): `no_margin`, `margin`, `margin_kept` and
        `margin_units`.  Each of 4 mutations (no signal, no survivor
        discount, sweep test ignores the margin, unique blobs not
        counted) is detected.
      - **Done.**  Scans of the atom array (`current_blob/2`,
        `read_term/2,3` with `blob(resolve)`) take a reference with
        `bump_scanned_references()`: our own local atoms without
        promoting them, local atoms of other threads not at all.  Their
        owner releases and reuses the slots without synchronising with
        the scan, so the scanned `references` and `type` may be
        outdated, and passing an outdated type to `promoting_ref()`
        mishandles `PL_BLOB_OWNED` references.  `blob(resolve)` finds a
        local `UNIQUE|NOCOPY` blob of another thread through the atom
        table (`localBlobsByAddress()`), where the bucket protocol makes
        promotion safe.  Tests (unit `local_find`): `enum_own`,
        `enum_other`, `resolve_own` and `resolve_other`; each of 4
        mutations is detected.
      - **Done.**  Stream handles no longer copy their key: they are
        `PL_BLOB_UNIQUE|PL_BLOB_NOCOPY` on the `IOSTREAM`.  A stream pair
        is a `stream_pair` record, found back from its streams in a list
        under `L_FILE`, so a pair keeps a unique handle.  The record has
        its magic (`SIO_PMAGIC`) at the offset of the `IOSTREAM` magic,
        which tells the two apart.  Releasing a pair only tries
        `L_FILE`: `stream_property/2` creates handles holding it, and the
        release may run with `L_REHASH_ATOMS`.  `blob(resolve)` now finds
        a stream by address.  Measured: 200,000 `open_string/2`,
        `read/2`, `close/1` take 0.120 s instead of 0.126–0.177 s.
        Tests: `stream_pair` unit of `tests/core_text/test_io.pl`
        (`unique`, `parts`, `distinct`, `no_pair`, `recreate`,
        `recreate_other`) and `blob_resolve:stream_pair`; removing the
        search for the record, the magic test or the unlink is
        detected.
      - **Done.**  Stream handles are `PL_BLOB_LOCAL`.  Another thread
        that finds the stream (`stream_property/2`, an alias) gets the
        same handle and promotes it; sending a handle promotes it.  The
        local sweep must not call Prolog, which closing a stream may do
        (`open_prolog_stream/4`).  So, with `agc_close_streams`, the
        release refuses if it would close an open stream while
        `LD->atoms.local.sweeping`, and a local atom whose release
        refuses is now promoted, which leaves it to global AGC.  This
        also promotes a record handle whose release finds `L_RECORD`
        busy.  Measured, 200,000 `open_string/2`, `read/2`, `close/1`:
        1 thread 0.125–0.154 s and no global AGC, instead of
        0.171–0.194 s and 17; 4 threads 3–6 global AGC runs instead of
        78–112, same time (0.36–0.85 s, bimodal in both).  Tests (unit
        `local_streams`): `local_handle`, `reclaimed`, `alias_promotes`
        and `no_local_close`; removing the flag, the refusal or the
        promotion of refused atoms is detected.
      - **Done.**  Undo goals (`undo/1`) are `PL_BLOB_NOCOPY|PL_BLOB_LOCAL`
        blobs on the record of the goal; they are not unique, as every
        call records its goal, so they skip the atom table.  The
        scheduled goals are registered with `registerAtomLocally()`.
        This needed a fix first (on master): `$undo/1` never dropped the
        reference that creating the blob returns, so undo blobs and
        their records were never reclaimed.  1,000,000 `undo(true)`
        calls followed by backtracking: 0.557 s, no global AGC, 999,554
        blobs reclaimed by the thread's sweep.  Test
        `undo:local_reclaimed`; dropping the flag or registering the
        scheduled goals with `PL_register_atom()` is detected.
      - **Done.**  Text atoms created by `unify_text()` (`PL_ATOM`), and
        thus by `PL_unify_chars()`, `atom_concat/3`, `atom_codes/2` and
        the other builtins that unify text with an atom, are local.
        `lookupBlobEx()` creates a local atom for a type without
        `PL_BLOB_LOCAL` if asked; `unify_text()` puts the atom on the
        stack and drops its reference.  They are in the atom table, so
        another thread looking up the same text finds and promotes them;
        storing them in clauses, the functor table, flags, records or
        tries promotes them.  Atoms created with a reference that the
        caller may keep (`PL_new_atom()`, `textToAtom()`, the reader,
        caches such as `for_code`) remain ordinary atoms.
          - 1,000,000 `atom_concat/3`, CPUs 0–3: one thread 0.12–0.17 s
            with no global AGC instead of 0.15–0.21 s with 58–61 runs
            and 0.13–0.20 s AGC CPU; 4 threads 0.31–0.34 s instead of
            0.42–0.44 s and 0.9–1.0 s AGC CPU.  Tabling benchmarks
            100%, ErgoAI cyc150 the same (29.6 s against 29.4–30.0 s),
            `bench/` 98.8% (geometric mean).
          - `answer_discard:atom_keep_discarded` expected no atoms to
            survive; in the debug build 3 now survive, marked by the
            conservative scan of stale local stack cells, as its sibling
            `atom_keep_deep` already allows.
          - Tests (unit `local_text`): `atom_concat`, `same_text`,
            `assert`, `functor`, `reclaimed`, `read_is_global`; making
            `unify_text()` create ordinary atoms is detected.  The
            `gc_margin` tests now count local sweeps as collections.
          - Risk: foreign code that passes an `atom_t` obtained from a
            term to another thread without `PL_register_atom()`.  That
            was always wrong, but global AGC forgave it because it
            scans all stacks.  The manual (`sec:atomgc`, "Passing atoms
            to other threads") warns about it; debug builds report the
            use through `check_atom_t()`.
      - **Done.**  The atoms of name tokens of read_term/2 are local
        (`textToLocalAtom()` in `get_token()`).  The reader keeps a
        reference while parsing, which does not promote; the atoms of
        functors are promoted by the functor table.  Reading 300,000
        facts with 4 new atoms each and discarding them: 0.36 s process
        CPU and no global AGC instead of 0.50–0.65 s and 91 AGC runs.
        Loading the same file is unchanged (1.86–1.90 s against
        1.85–1.87 s) after two fixes this exposed:
          - load_files/2 runs in sig_atomic/1, so the signal for the
            local sweep stayed pending: every call took the slow path
            and every new atom raised it again (12% slower).
            handleSignals() now does the local sweep in a critical
            section (`criticalAtomGC()`); global AGC still waits.
          - Atoms promoted by their owner counted as live until a
            sweep, causing about 2,000 sweeps that reclaimed nothing.
            `promoted_atom()` now discounts them.
          - Tests (unit `local_text`): `read`, `read_quoted`,
            `read_arg`, `read_functor`, `read_assert`,
            `read_reclaimed`, `sig_atomic`, `promoted`; each change is
            detected by reverting it.
      - **Done.**  Clauses of thread-local predicates keep their atoms
        local.  `compileClause()` registers the atoms of a clause for a
        `P_LOCALISED` definition with `registerAtomLocally()`
        (`registerClauseAtom()`), as does `'$clause_copy'/2` for a
        thread-local target.  This is safe because only the owner can
        access these clauses: `PL_get_clref()` and `PL_get_dbref()` raise
        `permission_error(access, thread_local_clause, Ref)` for a clause
        of another thread's local definition (`foreignLocalClause()`),
        and calling a continuation that refers to one raises
        `permission_error(call, thread_local_clause, Cont)`.  The owner
        is found without a new field: the master procedure's definition
        for the calling thread must be the clause's definition.
        Freeing a retracted clause from the gc thread drops the local
        reference atomically; at thread exit, atoms still referenced by
        clauses that clause GC has not freed become ordinary atoms.
          - 20 × (50,000 `atom_concat/3` + `assertz/1` into a
            thread-local predicate, then `retractall/1`): 0.60 s process
            CPU without AGC instead of 0.75 s.
          - The atoms are reclaimed once clause GC frees the retracted
            clauses, which may take two clause GC passes, as for any
            dynamic predicate.
          - **Done.**  Local clause GC: the owner frees the erased
            clauses of its thread-local predicates itself.  Retract
            counts them in `LD->clauses.local` instead of registering the
            definition with the global clause GC
            (`registerErasedClauses()`).  At a threshold it signals
            SIG_CLAUSE_GC to itself; `localClauseGC()` marks the
            generations of its own frames and `predicate_references`
            and calls `cleanDefinition()`.  Clause references unlinked
            during the pass go to a thread-local list and are freed at
            its end, rather than lingering until a global clause GC.  A
            definition the thread walks from C (`access.predicate`),
            definitions registered globally (destroyed ones) and
            transactions are left alone.  In a critical section the pass
            runs unless there is an event hook for erased clauses.
            1,000,000 `assertz/1`+`retract/1` of integers on a
            thread-local predicate: 0.81 s process CPU and no global
            clause GC instead of 1.62–1.70 s and about 680 runs.  The
            atom benchmark above: 0.47 s instead of 0.65 s.
          - `agcByGCThread()` became `gcByGCThread()`: `cgc_handler()`
            uses it to tell a local clause GC request from a global one.
          - Tests: `local_cgc`, `cgc_iterate`, `cgc_retract`,
            `cgc_indexed`, `cgc_choicepoint`, `cgc_sig_atomic`; always
            registering globally, not marking the own frames, and
            not handling the signal in critical sections are detected.
          - Tests (unit `local_thread_local`): `assert`, `retract`,
            `own_ref`, `reclaimed`, `clause`, `clause_property`,
            `nth_clause`, `erase`, `continuation`, `exit`; making the
            compiler register globally or dropping the access check is
            detected.
    Candidates:
      - core: streams, clause and record references, closures
        (`pl-wrap.c`), undo (`pl-undo.c`), zippers (`pl-zip.c`);
      - Janus: `PY_OBJECT`;
      - xpce: `pce_blob` (`UNIQUE|NOCOPY`, with a `gc_margin`), whose
        lifetime interacts with xpce's own reference counting;
      - **Done.**  JPL: `jref_blob` is `UNIQUE|LOCAL`.  Its release
        hook deletes a JNI global reference, now in the owner thread,
        which is attached to the JVM, rather than in the gc thread.
        Strings that Java returns and `org.jpl7.Atom` terms that Java
        puts into Prolog become atoms through `PL_unify_wchars()`, so
        new ones are local; `Prolog.new_atom()` keeps `PL_new_atom()`.
        200,000 `jpl_new('java.lang.Object', [], _)`: 0.54–0.71 s
        instead of 2.0 s and 19 AGCs; the reference table stays at a
        few hundred entries instead of about 6,000.  Release used to
        find its table entry by scanning the whole table; it now uses
        the slot of the object's `identityHashCode()` (50,000 live
        references, releasing 65,536 others: 0.28 s instead of 19 s).
        Tests: `jpl_local` in
        `test_jpl.pl`; dropping any of the three changes is detected;
      - packages: archive, bdb, memfile, socket, alarm (clib), pcre,
        semweb snapshots and turtle parsers, ssl/crypto contexts, tipc,
        yaml emitters.

    Each needs its own review of the escape paths, e.g. xpce objects
    passed to the GUI thread.
